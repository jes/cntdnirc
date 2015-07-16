package Cntdn::Nick;

use strict;
use warnings;

use List::Util qw(shuffle);

use base qw(Cntdn::Base);

my @vowels = split //, 'AAAAAAAAAAAAAAAEEEEEEEEEEEEEEEEEEEEEIIIIIIIIIIIIIOOOOOOOOOOOOOUUUUU';
my @consonants = split //, 'BBCCCDDDDDDFFGGGHHJKLLLLLMMMMNNNNNNNNPPPPQRRRRRRRRRSSSSSSSSSTTTTTTTTTVWXYZ';

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
    letters_answers => \&begin_letters_answers,
    letters_timer => \&begin_letters_timer,
    pick_letters => \&begin_pick_letters,
);

my %said_in_state = (
    pick_letters => \&pick_letters_said,
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

        $methods{$cmd}->($self, $args, @cmdargs) if $methods{$cmd};
    }

    $said_in_state{$self->{game}{state}}->($self, $args) if $said_in_state{$self->{game}{state}};
}

sub tick {
    my ($self) = @_;

    return unless $self->{game}{timer_end};

    if (time >= $self->{game}{timer_end}) {
        # timer finished, enter the next state
        $self->set_state($self->{game}{timer_state});
        return 0;
    } else {
        # wait some more before firing again
        return $self->{game}{timer_end} - time;
    }
}

### said_in_state hooks:

sub pick_letters_said {
    my ($self, $args) = @_;

    return unless $args->{who} eq $self->{game}{letters_picker};

    $self->pick_letter('vowel') if $args->{body} =~ /^\s*v(owel)?\s*/;
    $self->pick_letter('consonant') if $args->{body} =~ /^\s*c(onsonant)?\s*/;
}

### game state management:

sub reset {
    my ($self) = @_;

    $self->{game} = {};
    $self->{game}{letters_turn} = -1;
    $self->{game}{numbers_turn} = -1;
    $self->{game}{letters} = [];

    # TODO: allow different probabilities in different formats (e.g. bastard mode)
    $self->{game}{vowel_stack} = [];
    $self->{game}{consonant_stack} = [];

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

sub pick_letter {
    my ($self, $type) = @_;

    my $l;

    if ($type eq 'vowel') {
        $self->{game}{vowel_stack} = [shuffle @vowels] unless @{ $self->{game}{vowel_stack} };
        $l = shift @{ $self->{game}{vowel_stack} };
    } else {
        $self->{game}{consonant_stack} = [shuffle @consonants] unless @{ $self->{game}{consonant_stack} };
        $l = shift @{ $self->{game}{consonant_stack} };
    }

    push @{ $self->{game}{letters} }, $l;
    $self->say(
        channel => $self->channel,
        body => join(' ', map { uc $_ } @{ $self->{game}{letters} }),
    );

    # TODO: take number of letters from config file
    if (@{ $self->{game}{letters} } == 9) {
        $self->set_state('letters_timer');
    } else {
        $self->set_state('pick_letters');
    }
}

### command handlers:

sub begin_game {
    my ($self, $args) = @_;

    return unless $self->{game}{state} eq 'join';
    return unless $self->has_joined($args->{who});
    return unless @{ $self->{game}{players} };

    $self->{game}{players} = [shuffle @{ $self->{game}{players} }];

    $self->say(
        channel => $self->channel,
        body => "beginning game with " . join(', ', @{ $self->{game}{players} }),
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

    $self->{game}{letters_turn}++;
    $self->{game}{letters_turn} %= @{ $self->{game}{players} };
    $self->{game}{letters_picker} = $self->{game}{players}[$self->{game}{letters_turn}];

    $self->say(
        channel => $self->channel,
        body => "Letters round. It's $self->{game}{letters_picker}'s turn to pick letters.",
    );

    $self->set_state('pick_letters');
}

sub begin_letters_answers {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => "Time's up!",
    );
}

sub begin_letters_timer {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => "30 seconds to solve those letters...",
    );
    $self->{game}{timer_end} = time + 30;
    $self->{game}{timer_state} = 'letters_answers';

    $self->schedule_tick(30);
}

sub begin_pick_letters {
    my ($self) = @_;

    # TODO: maximum of 6 consonants; maximum of 5 vowels (from config)
    $self->say(
        address => 1,
        who => $self->{game}{letters_picker},
        channel => $self->channel,
        body => "vowel or consonant? [v/c]",
    );
}

1;
