package Cntdn::Nick;

use strict;
use warnings;

use base qw(Cntdn::Base);

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

    return unless $args->{channel} eq $self->channel;

    $self->log("$args->{who} said '$args->{body}' in $args->{channel}");

    if ($self->is_authed($args->{who})) {
        $self->say(
            channel => $self->channel,
            body => 'You\'re authed',
        );
    }
}

1;
