package Cntdn::Base;

use strict;
use warnings;

use Bot::BasicBot;
use YAML qw(LoadFile);

use base qw(Bot::BasicBot);

sub new {
    my ($pkg, %opts) = @_;

    defined $opts{cfg} or die "No cfg passed to ${pkg}->new\n";
    my $cfg = LoadFile($opts{cfg});

    my $self = $pkg->SUPER::new(%{ $cfg->{basic_bot} });
    $self->{cfg} = $cfg;

    return $self;
}

sub is_authed {
    my ($self, $who) = @_;
    return 1;
}

sub channel {
    my ($self) = @_;
    return $self->{cfg}{basic_bot}{channels}[0];
}

1;
