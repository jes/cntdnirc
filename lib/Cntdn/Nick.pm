package Cntdn::Nick;

use strict;
use warnings;

use Cntdn::Base;
use Cntdn::Colours;
use Cntdn::Numbers;
use Cntdn::Player;
use List::Util qw(shuffle);

use base qw(Cntdn::Base);

my @vowels = split //, 'AAAAAAAAAAAAAAAEEEEEEEEEEEEEEEEEEEEEIIIIIIIIIIIIIOOOOOOOOOOOOOUUUUU';
my @consonants = split //, 'BBCCCDDDDDDFFGGGHHJKLLLLLMMMMNNNNNNNNPPPPQRRRRRRRRRSSSSSSSSSTTTTTTTTTVWXYZ';

my @large_nums = (25, 50, 75, 100);
my @small_nums = (1,1, 2,2, 3,3, 4,4, 5,5, 6,6, 7,7, 8,8, 9,9, 10,10);

# states:
# wait - wait for someone to !start
# join - wait for people to !join, and then !go
# letters - initialise letters round and enter pick_letters
# pick_letters - pick the letters
# letters_timer - wait 30s
# letters_answers - collect the lengths of the words
# letters_words - collect the players' words
# numbers - initialise numbers round and ask for numbers
# numbers_timer - wait 30s
# numbers_answers - collect the numbers they reached
# numbers_sums - collect the players' sums

# only for methods that make sense in any state (or at least many states)
my %methods = (
    reset => \&reset_game,
    state => \&show_state,
);

my %begin_state = (
    join => \&begin_join,

    letters => \&begin_letters,
    pick_letters => \&begin_pick_letters,
    letters_timer => \&begin_letters_timer,
    letters_answers => \&begin_letters_answers,
    letters_words => \&begin_letters_words,

    numbers => \&begin_numbers,
    numbers_timer => \&begin_numbers_timer,
    numbers_answers => \&begin_numbers_answers,
    numbers_sums => \&begin_numbers_sums,
);

my %said_in_state = (
    pick_letters => \&pick_letters_said,
    letters_answers => \&letters_answers_said,
    letters_words => \&letters_words_said,

    numbers => \&numbers_said,
    numbers_answers => \&numbers_answers_said,
    numbers_sums => \&numbers_sums_said,

    wait => \&wait_said,
    join => \&join_said,
);

my %pm_in_state = (
    letters_words => \&letters_words_pm,
    numbers_sums => \&numbers_sums_pm,
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

    my $state = $g->{state};

    if (lc $args->{channel} eq lc $self->channel) {
        $self->log("$args->{who} said '$args->{body}'");

        if ($args->{body} =~ /^!/) {
            my ($cmd, @cmdargs) = split /\s+/, $args->{body};
            $cmd =~ s/^!//;

            $methods{$cmd}->($self, $args, @cmdargs) if $methods{$cmd};
        }

        $said_in_state{$state}->($self, $args) if $said_in_state{$state};
    } else {
        $pm_in_state{$state}->($self, $args) if $pm_in_state{$state};
    }
}

sub tick {
    my ($self) = @_;

    print STDERR "[tick] Tick.\n";

    my $timer_end = $self->{timer_end};
    return unless $timer_end;

    if (time >= $timer_end) {
        print STDERR "[tick] Ready for tick!\n";
        # timer finished, run the callback
        my $cb = $self->{timer_cb};
        $self->{timer_cb} = undef;
        print STDERR "[tick] " . ($cb ? 'have' : 'no') . " callback\n";
        $cb->() if $cb;
        return 0;
    } else {
        # wait some more before firing again
        print STDERR "[tick] Not ready for tick\n";
        return $timer_end - time;
    }
}

### said_in_state hooks:

sub pick_letters_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $picker = $g->{letters_picker};
    return unless lc $args->{who} eq lc $picker->{nick};

    my $s = lc $args->{body};
    $s =~ s/\s*//g;

    # do nothing if it isn't a string of c's and v's
    return if $s =~ /[^cv]/;

    while ((@{ $g->{letters} } < $g->{format}{num_letters}) && $s =~ /([cv])/g) {
        $self->pick_letter($1 eq 'v' ? 'vowel' : 'consonant');
    }

    $self->say(
        channel => $self->channel,
        body => COLOUR('white', 'blue') . ' ' .join(' ', map { uc $_ } @{ $g->{letters} }, (' ')x($g->{format}{num_letters} - @{ $g->{letters} })) . ' ' . RESET(),
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
    return unless lc $args->{who} eq lc $answerer->{nick};

    if ($args->{body} =~ /^\s*(\d+)\s*$/) {
        my $len = $1;

        if ($len > $g->{format}{num_letters}) {
            $self->say(
                address => 1,
                who => $args->{who},
                channel => $self->channel,
                body => "that's too many letters, type " . BOLD() . "0" . RESET() . " if you have no word; how many letters?",
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

sub letters_words_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $p = $self->player_by_nick($args->{who});
    return unless $p && $p->{need_word};

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => "please send your word via private message",
    );
}

sub letters_words_pm {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $p = $self->player_by_nick($args->{who});
    return unless $p && $p->{need_word};

    if ($args->{body} =~ /^\s*(!?\w+)\s*$/) {
        my $word = $1;
        if ($word eq '!skip') {
            $p->{letters_length} = 0;
            $p->{letters_word} = '';
            $p->{need_word} = 0;
        } else {
            # check the word is the declared length
            if (length $word != $p->{letters_length}) {
                $self->say(
                    address => 0,
                    who => $args->{who},
                    channel => 'msg',
                    body => "that's not " . BOLD() . "$p->{letters_length}" . RESET() . " letters, type " . BOLD() . "!skip" . RESET() . " if you have no word; what is your word?",
                );
                return;
            }

            # check they didn't use letters they don't have
            if (!$self->{words}->can_make($word, @{ $g->{letters} })) {
                $self->say(
                    address => 0,
                    who => $args->{who},
                    channel => 'msg',
                    body => "you can't make that word, type " . BOLD() . "!skip" . RESET() . " if you have no word; what is your word?",
                );
                return;
            }

            # check in dictionary
            # TODO: allow word if other players accept it even if not in dictionary?
            if (!$self->{words}->is_word($word)) {
                $self->say(
                    address => 0,
                    who => $args->{who},
                    channel => 'msg',
                    body => "that's not a legit word, sorry",
                );

                $p->{letters_length} = 0;
                $p->{letters_word} = '';
                $p->{need_word} = 0;
                $self->next_word_answer;
                return;
            }

            $p->{letters_word} = $word;
            $p->{need_word} = 0;
        }

        $self->say(
            channel => $self->channel,
            body => RESET() . "received " . BOLD() . "$args->{who}" . RESET() . "'s word",
        );

        $self->next_word_answer;
    }
}

sub numbers_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $picker = $g->{numbers_picker};
    return unless lc $args->{who} eq lc $picker->{nick};

    if ($args->{body} =~ /^\s*(\d+)\s*$/) {
        my $numlarge = $1;
        if ($numlarge < $g->{format}{min_large}) {
            $self->say(
                address => 1,
                who => $args->{who},
                channel => $self->channel,
                body => RESET() . "must have at least " . BOLD() . "$g->{format}{min_large}" . RESET() . " large; how many large? [$g->{format}{min_large}-$g->{format}{max_large}]",
            );
        } elsif ($numlarge > $g->{format}{max_large}) {
            $self->say(
                address => 1,
                who => $args->{who},
                channel => $self->channel,
                body => RESET() . "must have at most " . BOLD() . "$g->{format}{max_large}" . RESET() . " large; how many large? [$g->{format}{min_large}-$g->{format}{max_large}]",
            );
        } else {
            $self->pick_numbers($numlarge);
        }
    }
}

sub numbers_answers_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $answerer = $g->{numbers_answerer};
    return unless lc $args->{who} eq lc $answerer->{nick};

    if ($args->{body} =~ /^\s*(\d+)\s*$/) {
        my $num = $1;

        $answerer->{numbers_sum} = $num;
        if (++$g->{numbers_answers_done} == @{ $g->{players} }) {
            $self->set_state('numbers_sums');
        } else {
            $self->set_state('numbers_answers');
        }
    }
}

sub numbers_sums_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $p = $self->player_by_nick($args->{who});
    return unless $p && $p->{need_sum};

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => RESET() . "please send your answer via private message",
    );

}

sub numbers_sums_pm {
    my ($self, $args) = @_;
    my $g = $self->{game};

    my $p = $self->player_by_nick($args->{who});
    return unless $p && $p->{need_sum};

    if ($args->{body} =~ /^\s*!skip\s*$/) {
        $p->{numbers_sum} = undef;
        $p->{numbers_expr} = undef;
        $p->{need_sum} = 0;
        $self->next_sum_answer;
        return;
    }

    my ($ok, $failure) = $g->{numbers_obj}->is_solution($args->{body}, $p->{numbers_sum});
    if (!$ok) {
        $self->say(
            address => 0,
            who => $args->{who},
            channel => 'msg',
            body => RESET() . BOLD() . "$failure" . RESET() . ", type " . BOLD() . "!skip" . RESET() . " if you have no answer; what is your answer?",
        );

        return;
    }

    $p->{numbers_expr} = $args->{body};
    $p->{need_sum} = 0;

    $self->say(
        channel => $self->channel,
        body => RESET() . "Received " . BOLD() . "$args->{who}" . RESET() . "'s answer",
    );

    $self->next_sum_answer;
}

sub wait_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->start_game($args) if $args->{body} =~ /^!start(\s|$)/;
}

sub join_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->begin_game if $args->{body} eq '!go' && $self->has_joined($args->{who});
    $self->join_game($args) if $args->{body} eq '!join';
}

### game state management:

# XXX: can only have one timer at a time
sub delay {
    my ($self, $secs, $cb, %opts) = @_;

    my $end = time + $secs;

    if ($opts{compute_cb}) {
        # XXX: run after a delay of 0 to allow this function to return and the event loop to tick
        $self->delay(0, sub {
            # run the compute_cb
            $opts{compute_cb}->();
            delete $opts{compute_cb};

            # then delay for however much time is left and run the cb
            $secs = $end - time;
            $self->delay($secs, $cb, %opts);
        });

        return;
    }

    print STDERR "[tick] scheduled callback in $secs secs; " . ($self->{timer_cb} ? 'overwrite' : 'added') . " callback\n";

    # optionally, run some compute-intensive operation "while waiting for the timer"

    $self->{timer_end} = $end;
    $self->{timer_cb} = $cb;

    $self->schedule_tick($secs);
}

sub reset {
    my ($self) = @_;

    my $g = $self->{game} = {};
    $g->{players} = [];
    $g->{letters_turn} = -1;
    $g->{numbers_turn} = -1;

    $g->{vowel_stack} = [];
    $g->{consonant_stack} = [];

    $g->{large_stack} = [];
    $g->{small_stack} = [];

    $self->{timer_cb} = undef;

    $self->set_state('wait');
}

sub player_by_nick {
    my ($self, $who) = @_;
    my $g = $self->{game};

    for my $p (@{ $g->{players} }) {
        return $p if lc $p->{nick} eq lc $who;
    }
    return undef;
}

sub next_round {
    my ($self) = @_;
    my $g = $self->{game};

    my $next = shift @{ $g->{format}{rounds} };
    if (!defined $next) {
        $self->reset;
        $self->say(
            channel => $self->channel,
            body => INVERSE() . BOLD() . 'game over' . RESET(),
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
            body => RESET() . BOLD() . "$p->{nick} - $p->{score}" . RESET() . " points",
        );
    }
}

sub has_joined {
    my ($self, $who) = @_;
    my $g = $self->{game};

    return 1 if grep { lc $_->{nick} eq lc $who } @{ $g->{players} };
    return 0;
}

sub pick_letter {
    my ($self, $type) = @_;
    my $g = $self->{game};

    my $l;

    # TODO: take @vowels/@consonants from game format
    if ($type eq 'vowel') {
        $g->{vowel_stack} = [shuffle @vowels] unless @{ $g->{vowel_stack} };
        $l = shift @{ $g->{vowel_stack} };
    } else {
        $g->{consonant_stack} = [shuffle @consonants] unless @{ $g->{consonant_stack} };
        $l = shift @{ $g->{consonant_stack} };
    }

    push @{ $g->{letters} }, $l;
}

sub pick_number {
    my ($self, $numlarge) = @_;
    my $g = $self->{game};

    my $numsmall = $g->{format}{num_numbers} - $numlarge;
    if (@{ $g->{numbers} } >= $numsmall) {
        # pick a large
        $g->{large_stack} = [shuffle @large_nums] unless @{ $g->{large_stack} };
        unshift @{ $g->{numbers} }, shift @{ $g->{large_stack} };
    } else {
        # pick a small
        $g->{small_stack} = [shuffle @small_nums] unless @{ $g->{small_stack} };
        unshift @{ $g->{numbers} }, shift @{ $g->{small_stack} };
    }

    # TODO: print them right-to-left (like the old CLI game)
    $self->say(
        channel => $self->channel,
        body => COLOUR('white', 'blue') . ' ' . join(' ', @{ $g->{numbers} }) . ' ' . RESET(),
    );

    if (@{ $g->{numbers} } == $g->{format}{num_numbers}) {
        # pick the target
        $self->delay(1, sub {
            $g->{numbers_target} = 100 + int(rand(900));

            $g->{numbers_obj} = Cntdn::Numbers->new(
                numbers => $g->{numbers},
                target => $g->{numbers_target},
            );

            $self->say(
                channel => $self->channel,
                body => "The target number is " . COLOUR('green', 'black') . "$g->{numbers_target}" . RESET(),
            );

            $self->delay(1, sub {
                $self->set_state('numbers_timer');
            });
        });
    } else {
        # pick another number
        $self->delay(1, sub {
            $self->pick_number($numlarge);
        });
    }
}

sub pick_numbers {
    my ($self, $numlarge) = @_;
    my $g = $self->{game};

    $self->say(
        channel => $self->channel,
        body => "Selecting " . BOLD() . "$numlarge" . RESET() . " large and " . BOLD() . ($g->{format}{num_numbers} - $numlarge) . RESET() . " small",
    );

    $self->delay(1, sub {
        $self->pick_number($numlarge);
    });
}

sub begin_game {
    my ($self) = @_;
    my $g = $self->{game};

    # cancel the 5-minute game start timer
    $self->{timer_cb} = undef;

    $g->{players} = [shuffle @{ $g->{players} }];

    $self->say(
        channel => $self->channel,
        body => RESET() . "Beginning game with " . join(', ', map { BOLD() . $_->{nick} . RESET() } @{ $g->{players} }),
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
        body => RESET() . "you've joined the game (now got " . BOLD() . (scalar @{ $g->{players} }) . RESET() . " players)",
    );
}

sub start_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    # TODO: get formats from cfg (maybe with specified format)
    my %formats = (
        default => {
            rounds => [qw(letters letters letters numbers letters letters letters numbers conundrum)],

            num_letters => 9,
            letters_time => 30, # secs

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs
        },
        letters => {
            rounds => [qw(letters letters letters letters letters letters letters letters)],

            num_letters => 9,
            letters_time => 30, # secs

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs
        },
        numbers => {
            rounds => [qw(numbers numbers numbers numbers numbers numbers numbers numbers)],

            num_letters => 9,
            letters_time => 30, # secs

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs
        },
    );

    my @words = split /\s+/, $args->{body};
    my $format = $words[1] || 'default';

    $g->{format} = $formats{$format};
    $g->{format_name} = $format;

    $self->set_state('join');

    # start 5 minute timer to reset if nobody joins or begin if anyone does
    $self->delay(300, sub {
        if (@{ $g->{players} }) {
            $self->begin_game;
        } else {
            $self->say(
                channel => $self->channel,
                body => "No players joined, cancelled game.",
            );
            $self->reset;
        }
    });
}

sub next_word_answer {
    my ($self) = @_;
    my $g = $self->{game};

    $g->{need_words}--;
    return unless $g->{need_words} == 0;

    # give 3 seconds to build suspense and allow players to switch back to the channel
    $self->delay(3, sub {
        # get players ordered by score
        my @players = sort { $b->{letters_length} <=> $a->{letters_length} } @{ $g->{players} };
        my $maxlen = $players[0]{letters_length};
        my @winners;
        for my $p (@players) {
            if ($p->{letters_length} > 0) {
                $self->say(
                    channel => $self->channel,
                    body => RESET() . BOLD() . "$p->{nick}" . RESET() . "'s word was " . BOLD() . "$p->{letters_word}" . RESET(),
                );
            } else {
                $self->say(
                    channel => $self->channel,
                    body => RESET() . BOLD () . "$p->{nick}" . RESET() . " had no valid word", # TODO: (attempted "<invalid word>")
                );
            }

            push @winners, $p if $p->{letters_length} == $maxlen;
        }

        # announce winners
        # TODO: "it's a tie" if everyone scored the same
        # TODO: not a "winner" if nobody got a word
        $self->say(
            channel => $self->channel,
            body => RESET() . "Round winner" . (@winners > 1 ? 's are ' : ' is ') . join(' and ', map { BOLD() . $_->{nick} . RESET() } @winners) . "!",
        );

        # calculate points
        my $points = $maxlen;
        $points *= 2 if $maxlen == $g->{format}{num_letters};

        # add points to all the players with the longest word
        $_->{score} += $points for @winners;

        # show best word
        # TODO: mention if it's longer than the players got (if the same, show an alternative if possible)
        $self->say(
            channel => $self->channel,
            body => "Best word available was " . BOLD() . $self->{words}->best_word(@{ $g->{letters} }) . RESET(),
        );

        $self->delay(3, sub {
            $self->show_scores;
            # bit of a delay before starting the next round
            $self->delay(1, sub {
                $self->next_round;
            });
        });
    });
}

sub next_sum_answer {
    my ($self) = @_;
    my $g = $self->{game};

    $g->{need_sums}--;
    return unless $g->{need_sums} == 0;

    $self->delay(3, sub {
        # get players ordered by score
        $_->{numbers_diff} = $_->{numbers_sum} ? abs($g->{numbers_target} - $_->{numbers_sum}) : 2**64 for @{ $g->{players} };
        my @players = sort { $a->{numbers_diff} <=> $b->{numbers_diff} } @{ $g->{players} };
        my $bestdiff = $players[0]{numbers_diff};
        my @winners;
        for my $p (@players) {
            if ($p->{numbers_sum}) {
                $self->say(
                    channel => $self->channel,
                    body => RESET() . BOLD() . "$p->{nick}" . RESET() . "'s answer was " . BOLD() . "$p->{numbers_expr} = $p->{numbers_sum}" . RESET(),
                );
            } else {
                $self->say(
                    channel => $self->channel,
                    body => RESET() . BOLD() . "$p->{nick}" . RESET() . " had no valid answer", # TODO: show what he tried
                );
            }

            push @winners, $p if $p->{numbers_diff} == $bestdiff;
        }

        # announce winners
        # TODO: "it's a tie" if everyone scored the same
        # TODO: not a "winner" if nobody got a word
        $self->say(
            channel => $self->channel,
            body => RESET() . "Round winner" . (@winners > 1 ? 's are ' : ' is ') . join(' and ', map { BOLD() . $_->{nick} . RESET() } @winners) . "!",
        );

        # calculate points
        my $points = 0;
        $points = 10 if $bestdiff == 0;
        $points = 7 if $bestdiff > 0 && $bestdiff <= 5;
        $points = 5 if $bestdiff > 5 && $bestdiff <= 10;

        # add points to all the winners
        $_->{score} += $points for @winners;

        # TODO: show best answer
        # TODO: mention if it's closer than players (if the same, show an alternative? by magic)
        $self->say(
            channel => $self->channel,
            body => "Best answer was " . BOLD() . $g->{numbers_best_answer} . RESET(),
        );

        $self->delay(3, sub {
            $self->show_scores;
            # bit of a delay before starting the next round
            $self->delay(1, sub {
                $self->next_round;
            });
        });
    });
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
        body => RESET() . "have reset",
    );
}

sub show_state {
    my ($self, $args) = @_;
    my $g = $self->{game};

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => RESET() . "state=$g->{state}",
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
        body => "Starting a game with format " . BOLD() . "$g->{format_name}" . RESET() . ", join with " . BOLD() . "!join" . RESET() . ", begin with " . BOLD() . "!go" . RESET(),
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
        body => RESET() . BOLD() . "Letters round." . RESET() . " It's " . BOLD() . "$g->{letters_picker}{nick}" . RESET() . "'s turn to pick letters.",
    );

    $self->set_state('pick_letters');
}

sub begin_pick_letters {
    my ($self) = @_;
    my $g = $self->{game};

    # TODO: some sort of timeout (timer) (needs cancelling - pick random letters)
    # TODO: maximum of 6 consonants; maximum of 5 vowels (from format)
    $self->say(
        address => 1,
        who => $g->{letters_picker}{nick},
        channel => $self->channel,
        body => RESET() . "vowel or consonant? " . BOLD() . "[v/c]" . RESET(),
    );
}

sub begin_letters_timer {
    my ($self) = @_;
    my $g = $self->{game};

    my $secs = $g->{format}{letters_time};

    $self->say(
        channel => $self->channel,
        body => BOLD() . "$secs" . RESET() . " seconds to solve those letters...",
    );
    # TODO: show 20s, 10s, 3,2,1 (timer)
    $self->delay($secs, sub {
        $self->say(
            channel => $self->channel,
            body => "Time's up!",
        );

        $g->{letters_answers} = [(undef) x @{ $g->{players} }];
        $g->{letters_answers_done} = 0;
        $g->{letters_answers_turn} = $g->{letters_turn} - 1;

        $self->set_state('letters_answers');
    });
}

sub begin_letters_answers {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{letters_answers_turn}++;
    $g->{letters_answers_turn} %= @players;
    $g->{letters_answerer} = $players[$g->{letters_answers_turn}];

    # TODO: some sort of timeout (timer) (needs cancelling - set their length to 0)

    $self->say(
        address => 1,
        who => $g->{letters_answerer}{nick},
        channel => $self->channel,
        body => RESET() . 'how many letters?',
    );
}

sub begin_letters_words {
    my ($self) = @_;
    my $g = $self->{game};

    $_->{need_word} = 1 for @{ $g->{players} };
    $g->{need_words} = @{ $g->{players} };

    # TODO: some sort of timeout (timer) (needs cancelling - set their word to '' )

    $self->say(
        channel => $self->channel,
        body => "please send your words via private message",
    );

    for my $p (@{ $g->{players} }) {
        $self->say(
            address => 0,
            who => $p->{nick},
            channel => 'msg',
            body => RESET() . "what is your " . BOLD() . "$p->{letters_length}" . RESET() . "-letter word? " . COLOUR('white', 'blue') . ' ' .join(' ', map { uc $_ } @{ $g->{letters} }, (' ')x($g->{format}{num_letters} - @{ $g->{letters} })) . ' ' . RESET(),
        );
    }
}

sub begin_numbers {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{numbers_turn}++;
    $g->{numbers_turn} %= @players;
    $g->{numbers_picker} = $players[$g->{numbers_turn}];
    $g->{numbers} = [];

    $self->say(
        channel => $self->channel,
        body => RESET() . "Numbers round. It's " . BOLD() . "$g->{numbers_picker}{nick}" . RESET() . "'s turn to pick numbers.",
    );

    $self->say(
        address => 1,
        who => $g->{numbers_picker}{nick},
        channel => $self->channel,
        body => RESET() . "need " . BOLD() . "$g->{format}{num_numbers}" . RESET() . " numbers; how many large? [$g->{format}{min_large}-$g->{format}{max_large}]",
    );
}

sub begin_numbers_timer {
    my ($self) = @_;
    my $g = $self->{game};

    my $secs = $g->{format}{numbers_time};

    $self->say(
        channel => $self->channel,
        body => BOLD() . "$secs" . RESET() . " seconds to solve those numbers...",
    );
    # TODO: show 20s, 10s, 3,2,1 (timer)
    $self->delay($secs, sub {
        $self->say(
            channel => $self->channel,
            body => "Time's up!",
        );

        $g->{numbers_answers} = [(undef) x @{ $g->{players} }];
        $g->{numbers_answers_done} = 0;
        $g->{numbers_answers_turn} = $g->{numbers_turn} - 1;

        $self->set_state('numbers_answers');
    },
    compute_cb => sub {
        # this can take a while, so here we run it as the compute callback
        # while the timer is ticking
        $g->{numbers_best_answer} = $g->{numbers_obj}->best_answer;
    });
}

sub begin_numbers_answers {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{numbers_answers_turn}++;
    $g->{numbers_answers_turn} %= @players;
    $g->{numbers_answerer} = $players[$g->{numbers_answers_turn}];

    # TODO: some sort of timeout (timer) (needs cancelling - set their number to invalid)

    $self->say(
        address => 1,
        who => $g->{numbers_answerer}{nick},
        channel => $self->channel,
        body => RESET() . 'what number did you reach?',
    );
}

sub begin_numbers_sums {
    my ($self) = @_;
    my $g = $self->{game};

    $_->{need_sum} = 1 for @{ $g->{players} };
    $g->{need_sums} = @{ $g->{players} };

    # TODO: some sort of timeout (timer) (needs cancelling - set their sum to invalid)

    $self->say(
        channel => $self->channel,
        body => "please send your answers via private message",
    );

    for my $p (@{ $g->{players} }) {
        $self->say(
            address => 0,
            who => $p->{nick},
            channel => 'msg',
            body => RESET() . "what is your " . BOLD() . "$p->{numbers_sum}" . RESET() . " answer? (use infix notation) " . COLOUR('white', 'blue') . ' ' . join(' ', @{ $g->{numbers} }) . ' ' . RESET(),
        );
    }
}

sub begin_conundrum {
    my ($self) = @_;
    my $g = $self->{game};

    my @players = @{ $g->{players} };
    $g->{conundrum_turn}++;
    $g->{conundrum_turn} %= @players;
    $g->{conundrum} = [ split //, $self->{words}->conundrum ];

    # TODO: ...
}

1;
