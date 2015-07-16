package Cntdn::Nick;

use strict;
use warnings;

use base qw(Cntdn::Base);

my %methods = (
    go => \&begin_game,
    join => \&join_game,
    reset => \&reset_game,
    start => \&start_game,
    state => \&show_state,
);

my %begin_state = (
    join => \&begin_join,
    letters => \&begin_letters,
);

### Bot::BasicBot hooks:

sub init {
    my ($self) = @_;

    $self->reset;

    return 1;
}

sub said {
    my ($self, $args) = @_;

    $self->SUPER::said($args);
    return unless $self->channel eq $self->channel;

    $self->log("$args->{who} said '$args->{body}'");

    if ($args->{body} =~ /^!/) {
        my ($cmd, @cmdargs) = split /\s+/, $args->{body};
        $cmd =~ s/^!//;

        if ($methods{$cmd}) {
            $methods{$cmd}->($self, $args, @cmdargs);
        }
    }
}

### game state management:

sub reset {
    my ($self) = @_;

    $self->{game} = {};
    $self->set_state('wait');
}

sub next_round {
    my ($self) = @_;

    my $next = shift @{ $self->{game}{format} };
    if (!defined $next) {
        # TODO: game ends
        $self->reset;
        $self->say(
            channel => $self->channel,
            body => 'game over',
        );
        return;
    }

    $self->set_state($next);
}

sub has_joined {
    my ($self, $who) = @_;

    return 1 if grep { $_ eq $who } @{ $self->{game}{players} };
    return 0;
}

### command handlers:

sub begin_game {
    my ($self, $args) = @_;

    return unless $self->{game}{state} eq 'join';
    return unless $self->has_joined($args->{who});

    $self->say(
        channel => $self->channel,
        body => "beginning game with " . (scalar @{ $self->{game}{players} }) . " players",
    );

    $self->next_round;
}

sub join_game {
    my ($self, $args) = @_;

    return unless $self->{game}{state} eq 'join';
    return if $self->has_joined($args->{who});

    push @{ $self->{game}{players} }, $args->{who};

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "you've joined the game (now got " . (scalar @{ $self->{game}{players} }) . " players)",
    );
}

sub reset_game {
    my ($self, $args) = @_;

    return unless $self->is_op($self->channel, $args->{who});

    $self->reset;

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "have reset",
    );
}

sub start_game {
    my ($self, $args, @cmdargs) = @_;

    return unless $self->{game}{state} eq 'wait';

    if (@cmdargs) {
        # TODO: start a game with specified format
    }

    # TODO: get formats from cfg
    $self->{game}{format} = [qw(
        letters letters letters letters numbers letters letters letters letters
        numbers letters letters letters numbers conundrum
    )];
    $self->set_state('join');
    # TODO: start 5 minute timer to ->reset if nobody joins or begin if anyone does
}

sub show_state {
    my ($self, $args) = @_;

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "state=$self->{game}{state}",
    );
}

### state entry:

sub set_state {
    my ($self, $state) = @_;

    $self->{game}{state} = $state;
    $begin_state{$state}->($self) if $begin_state{$state};
}

sub begin_join {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => 'Starting a game, join with !join, begin with !go',
    );
}

sub begin_letters {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => 'Letters round. Whose turn is it to pick?',
    );
}

1;
