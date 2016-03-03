package Cntdn::Words;

use strict;
use warnings;

use Text::Levenshtein qw(distance);

sub new {
    my ($pkg, %opts) = @_;

    my $self = bless \%opts, $pkg;
    $self->load;

    return $self;
}

sub load {
    my ($self) = @_;

    die "no dictionary file specified" unless $self->{file};

    open(my $fh, '<', $self->{file})
        or die "can't read $self->{file}: $!\n";

    $self->{trie} = {};
    $self->{is_word} = {};
    $self->{have9} = {};
    $self->{words_of_len} = {};
    while (<$fh>) {
        chomp;
        $self->add_word($_);
    }

    return;
}

sub add_word {
    my ($self, $word) = @_;
    $word = lc $word;

    # add word to trie:
    my $n = $self->{trie};

    my @chars = split //, $word;
    for my $c (@chars) {
        $n->{$c} ||= {};
        $n = $n->{$c};
    }

    $n->{'$'} = 1;

    # add word to conundrum data structures:
    $self->{is_word}{$word} = 1;
    push @{ $self->{have9}{join '', sort(split(//, $word))} }, $word if length $word == 9;;
    push @{ $self->{words_of_len}{length $word} }, $word;
}

sub is_word {
    my ($self, $word) = @_;
    $word = lc $word;

    my $n = $self->{trie};

    my @chars = split //, $word;
    for my $c (@chars) {
        return 0 if !$n->{$c};
        $n = $n->{$c};
    }

    return $n->{'$'};
}

sub can_make {
    my ($self, $word, @letters) = @_;

    my %c;
    $c{lc $_}++ for @letters;
    $c{lc $_}-- for split //, $word;

    return 0 if grep { $_ < 0 } values %c;
    return 1;
}

sub _recurse_solve_letters {
    my ($self, $node, $used_letter, $cb, $answer, @letters) = @_;

    $cb->($answer) if $node->{'$'};
    return if length $answer == @letters;

    my %done;

    for my $i (0 .. $#letters) {
        my $c = lc $letters[$i];

        next if $used_letter->{$i} || $done{$c};

        if ($node->{$c}) {
            $used_letter->{$i} = 1;
            $done{$c} = 1;
            $self->_recurse_solve_letters($node->{$c}, $used_letter, $cb, $answer.$c, @letters);
            $used_letter->{$i} = 0;
        }
    }
}

sub solve_letters {
    my ($self, $cb, @letters) = @_;
    $self->_recurse_solve_letters($self->{trie}, {}, $cb, '', @letters);
}

sub best_word {
    my ($self, @letters) = @_;

    my $longest = '';

    $self->solve_letters(sub {
        my ($w) = @_;
        $longest = $w if length $w > length $longest;
    }, @letters);

    return $longest;
}

sub conundrum {
    my ($self) = @_;

    while (1) {
        my $w1 = $self->{words_of_len}{4}[rand @{ $self->{words_of_len}{4} } - 1];
        my $w2 = $self->{words_of_len}{5}[rand @{ $self->{words_of_len}{5} } - 1];

        next if substr($w1, -1, 1) eq substr($w2, 0, 1) or $self->{is_word}{"$w1$w2"} or $self->{is_word}{"$w2$w1"};
        my $w = "$w1$w2";
        my @possible_nines = @{ ($self->{have9}{join '', sort(split(//, $w))})||[] };
        if (@possible_nines == 1) {
            my @words;
            for my $word (@possible_nines) {
                push @words, $word if distance($word, "$w1$w2") > 4 && distance($word, "$w2$w1") > 4;
            }

            if (rand() < 0.5) {
                return "$w1$w2";
            } else {
                return "$w2$w1";
            }
        }
    }

}

1;
