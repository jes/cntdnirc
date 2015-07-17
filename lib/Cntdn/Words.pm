package Cntdn::Words;

use strict;
use warnings;

use JSON::XS qw(decode_json);

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
    while (<$fh>) {
        chomp;
        $self->add_word($_);
    }

    return;
}

sub add_word {
    my ($self, $word) = @_;

    my $n = $self->{trie};

    my @chars = split //, $word;
    for my $c (@chars) {
        $n->{$c} ||= {};
        $n = $n->{$c};
    }

    $n->{'$'} = 1;
}

sub is_word {
    my ($self, $word) = @_;

    my $n = $self->{trie};

    my @chars = split //, $word;
    for my $c (@chars) {
        return 0 if !$n->{$c};
        $n = $n->{$c};
    }

    return $n->{'$'};
}

1;
