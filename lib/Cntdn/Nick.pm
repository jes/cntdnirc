package Cntdn::Nick;

use strict;
use warnings;

use Cntdn::Base;
use Cntdn::Colours;
use Cntdn::Numbers;
use Cntdn::Player;
use List::Util qw(shuffle);

use base qw(Cntdn::Base);

my @vowels = split //, 'AAAAAAAAAAAAAAAAAAAEEEEEEEEEEEEEEEEEEEEEEEIIIIIIIIIIIIIIIIOOOOOOOOOOOOOOOUUUU';
my @consonants = split //, 'BBCCCCDDDDDDDFFGGGGHJKLLLLLMMMMMMMNNNNNNNNNPPPQRRRRRRRRRRRRSSSSSSSSSSSSTTTTTTTTTTTTVWXYZ';

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
# conundrum - initialise conundrum and compute letters
# conundrum_timer - wait 30s, and allow people to answer

# only for methods that make sense in any state (or at least many states)
my %methods = (
    reset => \&reset_game,
    state => \&show_state,
    check => \&check_word,
    leave => \&leave_game,
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

    conundrum => \&begin_conundrum,
    conundrum_timer => \&begin_conundrum_timer,
);

my %said_in_state = (
    pick_letters => \&pick_letters_said,
    letters_answers => \&letters_answers_said,
    letters_words => \&letters_words_said,

    numbers => \&numbers_said,
    numbers_answers => \&numbers_answers_said,
    numbers_sums => \&numbers_sums_said,

    conundrum_timer => \&conundrum_timer_said,

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

sub say {
    my ($self, @args) = @_;

    use Data::Dumper;
    print STDERR "say: " . Dumper(\@args);
    $self->SUPER::say(@args);
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

    return undef;
}

sub chanpart {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $self->has_joined($args->{who});

    my $p = $self->player_by_nick($args->{who});
    $self->remove_player($p);

    return undef;
}

sub kicked {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $self->has_joined($args->{kicked});

    my $p = $self->player_by_nick($args->{kicked});
    $self->remove_player($p);

    return undef;
}

sub userquit {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return unless $self->has_joined($args->{who});

    my $p = $self->player_by_nick($args->{who});
    $self->remove_player($p);

    return undef;
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
        next if $1 eq 'v' && $g->{num_vowels} >= $g->{format}{max_vowels};
        next if $1 eq 'c' && $g->{num_consonants} >= $g->{format}{max_consonants};
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
                body => RESET() . "that's too many letters, type " . BOLD() . "0" . RESET() . " if you have no word; how many letters?",
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
        body => RESET() . "please send your word via private message",
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
            $p->{tried_word} = $word;

            # check the word is the declared length
            if (length $word != $p->{letters_length}) {
                $self->say(
                    address => 0,
                    who => $args->{who},
                    channel => 'msg',
                    body => RESET() . "that's not " . BOLD() . "$p->{letters_length}" . RESET() . " letters, type " . BOLD() . "!skip" . RESET() . " if you have no word; what is your word?",
                );
                return;
            }

            # check they didn't use letters they don't have
            if (!$self->{words}->can_make($word, @{ $g->{letters} })) {
                $self->say(
                    address => 0,
                    who => $args->{who},
                    channel => 'msg',
                    body => RESET() . "you can't make that word, type " . BOLD() . "!skip" . RESET() . " if you have no word; what is your word?",
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
                    body => RESET() . "that's not a legit word, sorry",
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
            body => RESET() . "Received " . BOLD() . "$args->{who}" . RESET() . "'s word",
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
            $g->{large_stack} = [shuffle @large_nums];
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

sub conundrum_timer_said {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return if $g->{times_up};

    my $p = $self->player_by_nick($args->{who});
    return unless $p;

    my $word = $args->{body};

    $word =~ s/\s*//g;
    $word =~ s/[\!\?]$//;

    return if length $word != @{ $g->{conundrum} };

    if ($p->{has_answered_conundrum}) {
        $self->say(
            address => 1,
            who => $args->{who},
            channel => $self->channel,
            body => RESET() . "you've already tried, you may not try again.",
        );
        return;
    }

    $p->{has_answered_conundrum} = 1;

    # check they didn't use letters they don't have
    if (!$self->{words}->can_make($word, @{ $g->{conundrum} })) {
        $self->say(
            address => 1,
            who => $args->{who},
            channel => $self->channel,
            body => RESET() . "you can't make that word. " . BOLD() . ($self->{timer_end} - time) . RESET() . " seconds remain for everyone else...",
        );
        return;
    }

    # check in dictionary
    if (!$self->{words}->is_word($word)) {
        $self->say(
            address => 1,
            who => $args->{who},
            channel => $self->channel,
            body => RESET() . "that's not a legit word, sorry. " . BOLD() . ($self->{timer_end} - time) . RESET() . " seconds remain for everyone else...",
        );
        return;
    }

    # else, they got it right!
    $g->{times_up} = 1;
    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => RESET() . "let's take a look...",
    );

    $self->delay(1, sub {
        $self->say(
            channel => $self->channel,
            body => COLOUR('white', 'blue') . ' ' . join(' ', map { uc $_ } split //, $word) . ' ' . RESET(),
        );

        $self->say(
            channel => $self->channel,
            body => RESET() . "It's the right answer! " . BOLD() . "10" . RESET() . " points to " . BOLD() . $p->{nick} . RESET(),
        );

        $p->{score} += 10;

        $self->delay(3, sub {
            $self->show_scores;
            # bit of a delay before starting the next round
            $self->delay(1, sub {
                $self->next_round;
            });
        });
    });
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

    $g->{allow_check_word} = 1;

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

    $g->{round}++;

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

sub remove_player {
    my ($self, $p) = @_;
    my $g = $self->{game};
    my $state = $g->{state};

    my $delete_idx;
    for my $i (0 .. @{ $g->{players} }-1) {
        if (lc $g->{players}[$i]{nick} eq lc $p->{nick}) {
            $delete_idx = $i;
            last;
        }
    }

    splice @{ $g->{players} }, $delete_idx, 1;

    $self->say(
        channel => $self->channel,
        body => RESET() . BOLD() . "$p->{nick}" . RESET() . " has left the game (now got " . BOLD() . (scalar @{ $g->{players} }) . RESET() . " players)",
    );

    if (@{ $g->{players} } == 0 && $state ne 'join') {
        $self->reset;
        $self->say(
            channel => $self->channel,
            body => INVERSE() . BOLD() . 'game over' . RESET(),
        );
        return;
    }

    # hacky stuff... basically we need to handle every special case where we're waiting
    # on the player that has left
    my @players = @{ $g->{players} };
    if ($state eq 'pick_letters' and lc $g->{letters_picker}{nick} eq lc $p->{nick}) {
        # advance letters_picker to next one
        $g->{letters_turn} %= @players;
        $g->{letters_picker} = $players[$g->{letters_turn}];
        $self->set_state('pick_letters');
    } elsif ($state eq 'letters_answers' and lc $g->{letters_answerer}{nick} eq lc $p->{nick}) {
        # skip to next answerer
        $g->{letters_answers_turn}--;
        if ($g->{letters_answers_done} == @{ $g->{players} }) {
            $self->set_state('letters_words');
        } else {
            $self->set_state('letters_answers');
        }
    } elsif ($state eq 'letters_words') {
        # skip to next answerer
        $self->next_word_answer if $p->{need_word};
    } elsif ($state eq 'numbers' and lc $g->{numbers_picker}{nick} eq lc $p->{nick}) {
        # advance numbers picker to next one
        $g->{numbers_turn}--;
        $self->set_state('numbers');
    } elsif ($state eq 'numbers_answers' and lc $g->{numbers_answerer}{nick} eq lc $p->{nick}) {
        # skip to next answerer
        $g->{numbers_answers_turn}--;
        if ($g->{numbers_answers_done} == @{ $g->{players} }) {
            $self->set_state('numbers_sums');
        } else {
            $self->set_state('numbers_answers');
        }
    } elsif ($state eq 'numbers_sums') {
        # skip to next answerer
        $self->next_sum_answer if $p->{need_sum};
    }
}

sub pick_letter {
    my ($self, $type) = @_;
    my $g = $self->{game};

    # don't allow !check after we've started picking letters
    $g->{allow_check_word} = 0;

    my $l;

    # TODO: take @vowels/@consonants from game format
    if ($type eq 'vowel') {
        $g->{num_vowels}++;
        $g->{vowel_stack} = [shuffle @vowels] unless @{ $g->{vowel_stack} };
        $l = shift @{ $g->{vowel_stack} };
    } else {
        $g->{num_consonants}++;
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
                body => "The target number is " . COLOUR('black', 'red') . " $g->{numbers_target} " . RESET(),
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
            max_consonants => 6,
            max_vowels => 5,

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs

            conundrum_time => 30, # secs
        },
        '15rounds' => {
            rounds => [qw(letters letters numbers letters letters numbers letters letters numbers letters letters letters letters numbers conundrum)],

            num_letters => 9,
            letters_time => 30, # secs
            max_consonants => 6,
            max_vowels => 5,

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs

            conundrum_time => 30, # secs
        },
        letters => {
            rounds => [qw(letters letters letters letters letters letters letters letters)],

            num_letters => 9,
            letters_time => 30, # secs
            max_consonants => 6,
            max_vowels => 5,

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs

            conundrum_time => 30, # secs
        },
        numbers => {
            rounds => [qw(numbers numbers numbers numbers numbers numbers numbers numbers)],

            num_letters => 9,
            letters_time => 30, # secs
            max_consonants => 6,
            max_vowels => 5,

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs

            conundrum_time => 30, # secs
        },
        conundrum => {
            rounds => [qw(conundrum conundrum conundrum conundrum conundrum conundrum conundrum conundrum)],

            num_letters => 9,
            letters_time => 30, # secs
            max_consonants => 6,
            max_vowels => 5,

            num_numbers => 6,
            min_large => 0,
            max_large => 4,
            numbers_time => 30, # secs

            conundrum_time => 30, # secs
        },
    );

    my @words = split /\s+/, $args->{body};
    my $format = $words[1] || 'default';

    if (!$formats{$format}) {
        $self->say(
            channel => $self->channel,
            body => RESET() . "Sorry, no such format " . BOLD() . $format . RESET(),
        );
        return;
    }

    $g->{format} = $formats{$format};
    $g->{format_name} = $format;
    $g->{round} = 0;
    $g->{nrounds} = @{ $g->{format}{rounds} };

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

    # allow !check any time after we've received all the answers
    $g->{allow_check_word} = 1;

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
                    body => RESET() . BOLD () . "$p->{nick}" . RESET() . " had no valid word (attempted " . BOLD() . "$p->{tried_word}" . RESET() . ")",
                );
            }

            push @winners, $p if $p->{letters_length} == $maxlen;
        }

        # calculate points
        my $points = $maxlen;
        $points *= 2 if $maxlen == $g->{format}{num_letters};

        # announce winners
        # TODO: "it's a tie" if everyone scored the same
        # TODO: not a "winner" if nobody got a word
        $self->say(
            channel => $self->channel,
            body => RESET() . "Round winner" . (@winners > 1 ? 's are ' : ' is ') . join(' and ', map { BOLD() . $_->{nick} . RESET() } @winners) . "! " . BOLD() . $points . RESET() . " points.",
        );

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

        # calculate points
        my $points = 0;
        $points = 10 if $bestdiff == 0;
        $points = 7 if $bestdiff > 0 && $bestdiff <= 5;
        $points = 5 if $bestdiff > 5 && $bestdiff <= 10;

        # announce winners
        # TODO: "it's a tie" if everyone scored the same
        # TODO: not a "winner" if nobody got a word
        $self->say(
            channel => $self->channel,
            body => RESET() . "Round winner" . (@winners > 1 ? 's are ' : ' is ') . join(' and ', map { BOLD() . $_->{nick} . RESET() } @winners) . "! " . BOLD() . $points . RESET() . " points.",
        );

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

sub check_word {
    my ($self, $args, $word) = @_;
    my $g = $self->{game};

    return if !$g->{allow_check_word};

    my $is_word = $self->{words}->is_word($word);
    my $could_make = $self->{words}->can_make($word, @{ $g->{last_letters} });

    my $msg = '';

    if ($is_word && $could_make) {
        $msg = ' is a valid word, and could be made last letters round.';
    } elsif ($is_word && !$could_make) {
        $msg = ' is a valid word, but could not be made last letters round.';
    } elsif (!$is_word && $could_make) {
        $msg = ' is not a valid word, but could be made last letters round.';
    } elsif (!$is_word && !$could_make) {
        $msg = ' is not a valid word, and could not be made last letters round.';
    }

    $self->say(
        address => 1,
        who => $args->{who},
        channel => $self->channel,
        body => RESET() . BOLD () . $word . RESET() . $msg,
    );
}

sub leave_game {
    my ($self, $args) = @_;
    my $g = $self->{game};

    return if $g->{state} eq 'wait';
    return unless $self->has_joined($args->{who});

    my $p = $self->player_by_nick($args->{who});

    $self->remove_player($p);
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
        body => "Starting a game with format " . BOLD() . "$g->{format_name}" . RESET() . ", join with " . BOLD() . "!join" . RESET() . ", leave with " . BOLD() . "!leave" . RESET() . ", begin with " . BOLD() . "!go" . RESET(),
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
    $g->{num_vowels} = 0;
    $g->{num_consonants} = 0;

    $self->say(
        channel => $self->channel,
        body => RESET() . BOLD() . "($g->{round}/$g->{nrounds}) Letters round." . RESET() . " It's " . BOLD() . "$g->{letters_picker}{nick}" . RESET() . "'s turn to pick letters.",
    );

    $self->set_state('pick_letters');
}

sub begin_pick_letters {
    my ($self) = @_;
    my $g = $self->{game};

    # TODO: some sort of timeout (timer) (needs cancelling - pick random letters)
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

    $g->{last_letters} = $g->{letters};

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
    $_->{tried_word} = '' for @{ $g->{players} };
    $g->{need_words} = @{ $g->{players} };

    # TODO: some sort of timeout (timer) (needs cancelling - set their word to '' )

    $self->say(
        channel => $self->channel,
        body => "Please send your words via private message",
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
        body => RESET() . BOLD() . "($g->{round}/$g->{nrounds}) Numbers round." . RESET() . " It's " . BOLD() . "$g->{numbers_picker}{nick}" . RESET() . "'s turn to pick numbers.",
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
        body => "Please send your answers via private message",
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
    $g->{conundrum} = [ split //, $self->{words}->conundrum ];
    $g->{times_up} = 0;
    $_->{has_answered_conundrum} = 0 for @players;

    # TODO: announce "crucial" if appropriate
    $self->say(
        channel => $self->channel,
        body => RESET() . BOLD() . "($g->{round}/$g->{nrounds}) Countdown Conundrum." . RESET() . " Blurt out your answer in the channel once you've got it. Get ready...",
    );

    $self->delay(2, sub {
        $self->say(
            channel => $self->channel,
            body => COLOUR('white', 'blue') . ' ' . join(' ', map { uc $_ } @{ $g->{conundrum} }) . ' ' . RESET(),
        );

        $self->set_state('conundrum_timer');
    });
}

sub begin_conundrum_timer {
    my ($self) = @_;
    my $g = $self->{game};

    my $secs = $g->{format}{conundrum_time};

    $self->say(
        channel => $self->channel,
        body => BOLD() . "$secs" . RESET() . " seconds to solve the conundrum. First to answer gets it...",
    );
    # TODO: show 20s, 10s, 3,2,1 (timer)
    $self->delay($secs, sub {
        $self->say(
            channel => $self->channel,
            body => "Time's up! Nobody got the answer.",
        );

        $g->{times_up} = 1;

        $self->delay(1, sub {
            my $word = $self->{words}->best_word(@{ $g->{conundrum} });
            $self->say(
                channel => $self->channel,
                body => COLOUR('white', 'blue') . ' ' . join(' ', map { uc $_ } split //, $word) . ' ' . RESET(),
            );

            $self->delay(3, sub {
                $self->show_scores;
                # bit of a delay before starting the next round
                $self->delay(1, sub {
                    $self->next_round;
                });
            });
        });
    });
}

1;
