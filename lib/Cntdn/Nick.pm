package Cntdn::Nick;

use strict;
use warnings;

use List::Util qw(shuffle);

use base qw(Cntdn::Base);

my @vowels = split //, 'AAAAAAAAAAAAAAAEEEEEEEEEEEEEEEEEEEEEIIIIIIIIIIIIIOOOOOOOOOOOOOUUUUU';
my @consonants = split //, 'BBCCCDDDDDDFFGGGHHJKLLLLLMMMMNNNNNNNNPPPPQRRRRRRRRRSSSSSSSSSTTTTTTTTTVWXYZ';

# states:
# wait - wait for someone to !start
# join - wait for people to !join, and then !go
# letters - initialise letters round and enter pick_letters
# pick_letters - pick the letters
# letters_timer - wait 30s
# letters_end - end of timer, and enter letters_answers
# letters_answers - collect the lengths of the words
# letters_words - order the players and enter letters_word
# letters_word - collect the player words

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
    letters_end => \&begin_letters_end,
    letters_timer => \&begin_letters_timer,
    letters_word => \&begin_letters_word,
    letters_words => \&begin_letters_words,
    pick_letters => \&begin_pick_letters,
);

my %said_in_state = (
    pick_letters => \&pick_letters_said,
    letters_answers => \&letters_answers_said,
    letters_word => \&letters_word_said,
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

sub letters_answers_said {
    my ($self, $args) = @_;

    return unless $args->{who} eq $self->{game}{letters_answerer};

    if ($args->{body} =~ /^\s*(\d+)\s*$/) {
        $self->{game}{letters_answers}[$self->{game}{letters_answers_turn}] = $1;
        if (++$self->{game}{letters_answers_done} == @{ $self->{game}{players} }) {
            $self->set_state('letters_words');
        } else {
            $self->set_state('letters_answers');
        }
    }
}

sub letters_word_said {
    my ($self, $args) = @_;

    return unless $args->{who} eq $self->{game}{player_answerer};

    if ($args->{body} =~ /^\s*(\w+)\s*$/) {
        # TODO: validate length
        # TODO: check in dictionary (allow word if other players accept it even if not in dictionary)
        $self->say(
            address => 1,
            who => $args->{who},
            channel => $self->channel,
            body => 'thanks',
        );

        if (@{ $self->{game}{player_answer_order} }) {
            $self->set_state('letters_word');
        } else {
            # TODO: announce the scores
            $self->next_round;
        }
    }
}

### game state management:

sub reset {
    my ($self) = @_;

    $self->{game} = {};
    $self->{game}{letters_turn} = -1;
    $self->{game}{numbers_turn} = -1;

    # TODO: allow different probabilities in different formats (e.g. bastard mode)
    $self->{game}{vowel_stack} = [];
    $self->{game}{consonant_stack} = [];

    $self->set_state('wait');
}

sub next_round {
    my ($self) = @_;

    my $next = shift @{ $self->{game}{format}{rounds} };
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

    if (@{ $self->{game}{letters} } == $self->{game}{format}{num_letters}) {
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
    $self->{game}{format}{rounds} = [qw(
        letters letters letters letters numbers letters letters letters letters
        numbers letters letters letters numbers conundrum
    )];
    $self->{game}{format}{num_letters} = 9;
    $self->{game}{format}{letters_time} = 3;
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
    $self->{game}{letters} = [];

    $self->say(
        channel => $self->channel,
        body => "Letters round. It's $self->{game}{letters_picker}'s turn to pick letters.",
    );

    $self->set_state('pick_letters');
}

sub begin_letters_answers {
    my ($self) = @_;

    $self->{game}{letters_answers_turn}++;
    $self->{game}{letters_answers_turn} %= @{ $self->{game}{players} };
    $self->{game}{letters_answerer} = $self->{game}{players}[$self->{game}{letters_answers_turn}];

    $self->say(
        address => 1,
        who => $self->{game}{letters_answerer},
        channel => $self->channel,
        body => 'how many letters?',
    );
}

sub begin_letters_end {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => "Time's up!",
    );

    $self->{game}{letters_answers} = [(undef) x @{ $self->{game}{players} }];
    $self->{game}{letters_answers_done} = 0;
    $self->{game}{letters_answers_turn} = $self->{game}{letters_turn} - 1;

    $self->set_state('letters_answers');
}

sub begin_letters_timer {
    my ($self) = @_;

    $self->say(
        channel => $self->channel,
        body => "$self->{game}{format}{letters_time} seconds to solve those letters...",
    );
    $self->{game}{timer_end} = time + $self->{game}{format}{letters_time};
    $self->{game}{timer_state} = 'letters_end';

    $self->schedule_tick($self->{game}{format}{letters_time});
}

sub begin_letters_word {
    my ($self) = @_;

    $self->{game}{player_answer_idx} = shift @{ $self->{game}{player_answer_order} };
    $self->{game}{player_answerer} = $self->{game}{players}[$self->{game}{player_answer_idx}];

    # TODO: some sort of timeout

    $self->say(
        address => 1,
        who => $self->{game}{player_answerer},
        channel => $self->channel,
        body => "what is your $self->{game}{letters_answers}[$self->{game}{player_answer_idx}]-letter word?",
    );
}

sub begin_letters_words {
    my ($self) = @_;

    # ask the player with the fewest letters first
    # TODO:  test all the stuff that involves multiplayer better
    $self->{game}{player_answer_order} = [sort {
        $self->{game}{letters_answers}[$a] <=> $self->{game}{letters_answers}[$b]
    } (0 .. @{ $self->{game}{players} }-1)];

    $self->set_state('letters_word');
}

sub begin_pick_letters {
    my ($self) = @_;

    # TODO: some sort of timeout
    # TODO: maximum of 6 consonants; maximum of 5 vowels (from format)
    $self->say(
        address => 1,
        who => $self->{game}{letters_picker},
        channel => $self->channel,
        body => "vowel or consonant? [v/c]",
    );
}

1;
