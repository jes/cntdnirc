package Cntdn::Numbers;

use strict;
use warnings;

sub new {
    my ($pkg, %opts) = @_;

    my $self = bless \%opts, $pkg;

    return $self;
}

sub is_solution {
    my ($self, $expr, $target) = @_;

    # check they don't use any disallowed characters
    return (0, 'invalid expression') if $expr =~ /[^0-9+*\-\/\(\) ]/ || $expr =~ /\*\*/;

    # check they don't use any numbers they don't have
    # HACK: this works by replacing the numbers with dots and checking that
    # there are no digits left after it's done
    my $nonums = ".$expr.";
    print STDERR "nonums=$nonums\n";
    for my $n (@{ $self->{numbers} }) {
        $nonums =~ s/\D\Q$n\E\D/./;
        print STDERR "nonums=$nonums\n";
    }
    return (0, 'you used excess numbers') if $nonums =~ /\d/;

    # XXX: do we need more sanitising?
    my $r = eval($expr);
    return (0, 'invalid expression') if $@;

    return (0, "that doesn't make that") if $r != $target;

    return (1, 'ok');
}

my %OPS = (
    "+" => sub { my ($n1, $n2) = @_; return undef if $n1<0 || $n2<0; return $n1+$n2; },
    "-" => sub { my ($n1, $n2) = @_; return undef if $n2 >= $n1; return $n1-$n2; },
    "_" => sub { my ($n2, $n1) = @_; return undef if $n2 >= $n1; return $n1-$n2; },
    "*" => sub { my ($n1, $n2) = @_; return $n1*$n2; },
    "/" => sub { my ($n1, $n2) = @_; return undef if $n2 == 0 || $n1%$n2 != 0; return $n1/$n2; },
    "?" => sub { my ($n2, $n1) = @_; return undef if $n2 == 0 || $n1%$n2 != 0; return $n1/$n2; },
);

my %OPCOST = (
    "+" => 1,
    "-" => 1.05,
    "_" => 1.05,
    "*" => 1.2,
    "/" => 1.3,
    "?" => 1.3,
);

sub _recurse_solve_numbers {
    my ($self, $numbers, $searchedi, $was_generated, $target, $levels, $valsums, $trickshot) = @_;
    $levels--;

    for my $i (0 .. @{ $numbers }-2) {
        my $ni = $numbers->[$i];
        next if !defined $ni;

        $numbers->[$i] = undef;

        for my $j ($i+1 .. @{ $numbers }-1) {
            my $nj = $numbers->[$j];
            next if !defined $nj;

            next if $i < $searchedi && !$was_generated->[$i] && !$was_generated->[$j];

            for my $o (keys %OPS) {
                my $r = $OPS{$o}($ni->[0], $nj->[0]);
                next if !defined $r;

                my $op_cost = abs($r);
                $op_cost /= 10 while $op_cost % 10 == 0 && $op_cost != 0;
                $op_cost = 1 if ($ni->[0] == 10 || $nj->[0] == 10) && $o eq '*';
                $op_cost *= $OPCOST{$o};

                my $newvalsums = $valsums + $op_cost;

                if ((abs($r - $target) < abs($self->{bestresult}[0] - $target))
                        || (abs($r - $target) == abs($self->{bestresult}[0] - $target) && ($trickshot || $newvalsums < $self->{bestvalsums}))) {
                    $self->{bestresult} = [$r,$o,$ni,$nj];
                    $self->{bestvalsums} = $newvalsums;
                }

                $numbers->[$j] = [$r, $o, $ni, $nj];
                my $old_was_gen = $was_generated->[$j];
                $was_generated->[$j] = 1;

                if ($levels > 0 && ($trickshot || $self->{bestresult}[0] != $target || $newvalsums < $self->{bestvalsums})) {
                    $self->_recurse_solve_numbers($numbers, $i+1, $was_generated, $target, $levels, $newvalsums, $trickshot);
                }

                $was_generated->[$j] = $old_was_gen;
                $numbers->[$j] = $nj;
            }
        }

        $numbers->[$i] = $ni;
    }
}

sub _recurse_stringify_result {
    my ($self, $result, $parent_op) = @_;

    my %swap = (
        "?" => "/",
        "_" => "-",
    );

    my %compatible = (
        "+" => {
            "+" => 1,
            "-" => 1,
        },
        "-" => {
            "+" => 1,
            "-" => 1,
        },
        "*" => {
            "*" => 1,
        },
        "." => {
            "+" => 1,
            "-" => 1,
            "*" => 1,
            "/" => 1,
        },
    );

    return $result->[0] if !$result->[1];

    if ($swap{$result->[1]}) {
        my $r = $result->[2];
        $result->[2] = $result->[3];
        $result->[3] = $r;
        $result->[1] = $swap{$result->[1]};
    }

    my $op = $result->[1];
    my $one = $result->[2];
    my $two = $result->[3];

    $parent_op ||= '.';
    my $lparen = $compatible{$parent_op}{$op} ? '' : '(';
    my $rparen = $compatible{$parent_op}{$op} ? '' : ')';

    return $lparen . $self->_recurse_stringify_result($one, $op) . " $op " . $self->_recurse_stringify_result($two, $op) . $rparen;
}

sub stringify_result {
    my ($self, $result) = @_;

    return $self->_recurse_stringify_result($result) . " = $result->[0]";
}

sub _solve_numbers {
    my ($self, $numbers, $target, $trickshot) = @_;

    $numbers = [map { [$_, undef] } @$numbers];

    my @was_generated;
    for my $i (0 .. @$numbers-1) {
        push @was_generated, undef;
    }

    $self->_recurse_solve_numbers($numbers, 0, \@was_generated, $target, scalar(@$numbers), 0, $trickshot);

    return $self->{bestresult};
}

sub solve_numbers {
    my ($self, %opts) = @_;

    my @numbers = sort @{ $self->{numbers} };
    $self->{bestresult} = [$numbers[0], undef];
    $self->{bestvalsums} = $numbers[0];

    # see if one of these numbers is the answer; with trickshot you'd rather
    # have an interesting answer that's close than an exact answer
    if (!$opts{trickshot}) {
        for my $i (1 .. @numbers-1) {
            if (abs($numbers[$i] - $self->{target}) < abs($self->{bestresult}[0] - $self->{target})) {
                $self->{bestresult} = [$numbers[$i], $numbers[$i]];
                $self->{bestvalsums} = $numbers[$i];
            }
        }
        if ($self->{bestresult}[0] == $self->{target}) {
            return $self->{target} + " = " + $self->{target};
        }
    }

    return $self->stringify_result($self->_solve_numbers(\@numbers, $self->{target}, $opts{trickshot}));
}

sub best_answer {
    my ($self) = @_;

    return $self->solve_numbers;
}

1;
