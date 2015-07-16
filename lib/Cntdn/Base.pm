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

sub is_op {
    my ($self, $channel, $who) = @_;

    my $names = $self->channel_data($channel);
    use Data::Dumper;
    print Dumper($names);
    return $names->{$who}{op};
}

sub channel {
    my ($self) = @_;
    return $self->{cfg}{channel};
}

sub connected {
    my ($self) = @_;

    $self->say(
        who => 'NickServ',
        channel => 'msg',
        body => "IDENTIFY $self->{cfg}{ns_name} $self->{cfg}{ns_id}",
    );
}

sub said {
    my ($self, $args) = @_;

    use Data::Dumper;
    print Dumper($args);

    if ($args->{who} eq 'NickServ' and $args->{body} =~ /^You are now identified/) {
        $self->pocoirc->yield(join => $self->channel);
    }
}

1;
