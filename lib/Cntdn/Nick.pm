package Cntdn::Nick;

use strict;
use warnings;

use Cntdn::Base;
use Cntdn::Player;
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

# only for methods that make sense in any state (or at least many states)
my %methods = (
    reset => \&reset_game,
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
    wait => \&wait_said,
    join => \&join_said,
);

### Bot::BasicBot hooks:

sub init {
    my ($self) = @_;

    $self->reset;

    return 1;
}

sub said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->SUPER::said($args);
    return unless $self->channel eq $self->channel;

    $self->log("$args->{who} said '$args->{body}'");

    if ($args->{body} =~ /^!/) {
        my ($cmd, @cmdargs) = split /\s+/, $args->{body};
        $cmd =~ s/^!//;

        $methods{$cmd}->($self, $args, @cmdargs) if $methods{$cmd};
    }

    my $state = $g->{state};
    $said_in_state{$state}->($self, $args) if $said_in_state{$state};
}

sub tick {
    my ($self) = @_;
    my $g = $self->{game};

    my $timer_end = $g->{timer_end};
    return unless $timer_end;

    if (time >= $timer_end) {
        # timer finished, enter the next state
        $self->set_state($g->{timer_state});
        return 0;
    } else {
        # wait some more before firing again
        return $timer_end - time;
    }
}

### said_in_state hooks:

sub pick_letters_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $picker = $g->{letters_picker};
    return unless $args->{who} eq $picker->{nick};

    my $s = $args->{body};
    $s =~ s/\s*//g;

    # do nothing if it isn't a string of c's and v's
    return if $s =~ /[^cv]/;

    while ((@{ $g->{letters} } < $g->{format}{num_letters}) && $s =~ /([cv])/g) {
        $self->pick_letter($1 eq 'v' ? 'vowel' : 'consonant');
    }

    $self->say(
        channel => $self->channel,
        body => join(' ', map { uc $_ } @{ $g->{letters} }),
    );

    if (@{ $g->{letters} } == $g->{format}{num_letters}) {
        $self->set_state('letters_timer');
    } else {
        $self->set_state('pick_letters');
    }
}

sub letters_answers_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $answerer = $g->{letters_answerer};
    return unless $args->{who} eq $answerer->{nick};

    if ($args->{body} =~ /^\s*(\d+)\s*$/) {
        my $len = $1;

        if ($len > $g->{format}{num_letters}) {
            $self->say(
                address => 1,
                who => $args->{who},
                channel => $self->channel,
                body => "that's too many letters, type 0 if you have no word",
            );
            return;
        }

        $answerer->{letters_length} = $len;
        if (++$g->{letters_answers_done} == @{ $g->{players} }) {
            $self->set_state('letters_words');
        } else {
            $self->set_state('letters_answers');
        }
    }
}

sub letters_word_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $answerer = $g->{player_answerer};
    return unless $args->{who} eq $answerer->{nick};

    if ($args->{body} =~ /^\s*(!?\w+)\s*$/) {
        my $word = $1;
        if ($word eq '!skip') {
            $answerer->{letters_length} = 0;
            $answerer->{letters_word} = '';
        } else {
            if (length $word != $answerer->{letters_length}) {
                $self->say(
                    address => 1,
                    who => $args->{who},
                    channel => $self->channel,
                    body => "that's not $answerer->{letters_length} letters, type !skip if you have no word",
                );
                return;
            }

            # TODO: check in dictionary (TODO: allow word if other players accept it even if not in dictionary)
            $self->say(
                channel => $self->channel,
                body => "not validating the word (TODO)",
            );

            $answerer->{letters_word} = $word;
        }

        $self->say(
            address => 1,
            who => $args->{who},
            channel => $self->channel,
            body => 'thanks',
        );

        $self->next_word_answer;
    }
}

sub wait_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->start_game($args) if $args->{body} eq '!start';
}

sub join_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->begin_game($args) if $args->{body} eq '!go';
    $self->join_game($args) if $args->{body} eq '!join';
}

### game state management:

sub reset {
    my ($self) = @_;

    my $g = $self->{game} = {};
    $g->{letters_turn} = -1;
    $g->{numbers_turn} = -1;

    # TODO: allow different probabilities in different formats (e.g. bastard mode)
    $g->{vowel_stack} = [];
    $g->{consonant_stack} = [];

    $self->set_state('wait');
}

sub next_round {
    my ($self) = @_;
    my $g = $self->{game};

    my $next = shift @{ $g->{format}{rounds} };
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

sub show_scores {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = sort {
        $b->{score} <=> $a->{score};
    } @{ $g->{players} };

    for my $p (@players) {
        $self->say(
            channel => $self->channel,
            body => "$p->{nick} - $p->{score} points",
        );
    }
}

sub has_joined {
    my ($self, $who) = @_;
    my $g = $self->{game};

    return 1 if grep { $_->{nick} eq $who } @{ $g->{players} };
    return 0;
}

sub pick_letter {
    my ($self, $type) = @_;
    my $g = $self->{game};

    my $l;

    if ($type eq 'vowel') {
        $g->{vowel_stack} = [shuffle @vowels] unless @{ $g->{vowel_stack} };
        $l = shift @{ $g->{vowel_stack} };
    } else {
        $g->{consonant_stack} = [shuffle @consonants] unless @{ $g->{consonant_stack} };
        $l = shift @{ $g->{consonant_stack} };
    }

    push @{ $g->{letters} }, $l;
}

sub begin_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $g->{state} eq 'join';
    return unless $self->has_joined($args->{who});
    return unless @{ $g->{players} };

    $g->{players} = [shuffle @{ $g->{players} }];

    $self->say(
        channel => $self->channel,
        body => "beginning game with " . join(', ', map { $_->{nick} } @{ $g->{players} }),
    );

    $self->next_round;
}

sub join_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $g->{state} eq 'join';
    return if $self->has_joined($args->{who});

    push @{ $g->{players} }, Cntdn::Player->new(
        nick => $args->{who},
        score => 0,
    );

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "you've joined the game (now got " . (scalar @{ $g->{players} }) . " players)",
    );
}

sub start_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $g->{state} eq 'wait';

    # TODO: get formats from cfg (maybe with specified format)
    $g->{format} = {
        rounds => [qw(
            letters letters letters letters numbers letters letters letters letters
            numbers letters letters letters numbers conundrum
        )],
        num_letters => 9,
        letters_time => 3,
    };
    $self->set_state('join');
    # TODO: start 5 minute timer to ->reset if nobody joins or begin if anyone does
}

sub next_word_answer {
    my ($self) = @_;
    my $g = $self->{game};

    if (@{ $g->{player_answer_order} }) {
        $self->set_state('letters_word');
    } else {
        # get players ordered by score
        my @players = @{ $g->{players} };
        my $maxlen = 0;
        for my $p (@players) {
            $maxlen = $p->{letters_length} if $p->{letters_length} > $max;
        }

        # calculate points
        my $points = $maxlen;
        $points *= 2 if $maxlen == $g->{format}{num_letters};

        # add points to all the players with the longest word
        for my $p (@players) {
            $p->{score} += $points if $p->{letters_length} == $maxlen;
        }

        $self->show_scores;
        $self->next_round;
    }
}


### command handlers:

sub reset_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $self->is_op($self->channel, $args->{who});

    $self->reset;

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "have reset",
    );
}

sub show_state {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "state=$g->{state}",
    );
}

### state entry:

sub set_state {
    my ($self, $state) = @_;
    my $g = $self->{game};

    $g->{state} = $state;
    $begin_state{$state}->($self) if $begin_state{$state};
}

sub begin_join {
    my ($self) = @_;
    my $g = $self->{game};

    $self->say(
        channel => $self->channel,
        body => 'Starting a game, join with !join, begin with !go',
    );
}

sub begin_letters {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{letters_turn}++;
    $g->{letters_turn} %= @players;
    $g->{letters_picker} = $players[$g->{letters_turn}];
    $g->{letters} = [];

    $self->say(
        channel => $self->channel,
        body => "Letters round. It's $g->{letters_picker}{nick}'s turn to pick letters.",
    );

    $self->set_state('pick_letters');
}

sub begin_letters_answers {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{letters_answers_turn}++;
    $g->{letters_answers_turn} %= @players;
    $g->{letters_answerer} = $players[$g->{letters_answers_turn}];

    $self->say(
        address => 1,
        who => $g->{letters_answerer}{nick},
        channel => $self->channel,
        body => 'how many letters?',
    );
}

sub begin_letters_end {
    my ($self) = @_;
    my $g = $self->{game};

    $self->say(
        channel => $self->channel,
        body => "Time's up!",
    );

    $g->{letters_answers} = [(undef) x @{ $g->{players} }];
    $g->{letters_answers_done} = 0;
    $g->{letters_answers_turn} = $g->{letters_turn} - 1;

    $self->set_state('letters_answers');
}

sub begin_letters_timer {
    my ($self) = @_;
    my $g = $self->{game};

    my $secs = $g->{format}{letters_time};

    $self->say(
        channel => $self->channel,
        body => "$secs seconds to solve those letters...",
    );
    $g->{timer_end} = time + $secs;
    $g->{timer_state} = 'letters_end';

    $self->schedule_tick($secs);
}

sub begin_letters_word {
    my ($self) = @_;
    my $g = $self->{game};

    $g->{player_answer_idx} = shift @{ $g->{player_answer_order} };
    $g->{player_answerer} = $g->{players}[$g->{player_answer_idx}];

    # skip this player if they have 0 letters
    if ($g->{player_answerer}{letters_length} == 0) {
        $self->next_word_answer;
        return;
    }

    # TODO: some sort of timeout

    $self->say(
        address => 1,
        who => $g->{player_answerer}{nick},
        channel => $self->channel,
        body => "what is your $g->{player_answerer}{letters_length}-letter word?",
    );
}

sub begin_letters_words {
    my ($self) = @_;
    my $g = $self->{game};

    # ask the player with the fewest letters first
    my @players = @{ $g->{players} };
    $g->{player_answer_order} = [sort {
        $players[$a]{letters_length} <=> $players[$b]{letters_length}
    } (0 .. @players-1)];

    $self->set_state('letters_word');
}

sub begin_pick_letters {
    my ($self) = @_;
    my $g = $self->{game};

    # TODO: some sort of timeout
    # TODO: maximum of 6 consonants; maximum of 5 vowels (from format)
    $self->say(
        address => 1,
        who => $g->{letters_picker}{nick},
        channel => $self->channel,
        body => "vowel or consonant? [v/c]",
    );
}

1;
