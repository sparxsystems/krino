:- module(krino_english_grammar,
    [
       
    ]).

% Eliminate the left recusion infinite loop.
:- table sentence/3.
:- table clause/3.
:- table subject/3.
:- table predicate/3.
:- table noun_phrase/3.
:- table adjective_phrase/3.
:- table adverb_phrase/3.


sentence(Sentence) --> first_independent_clause(Clause), punctuation_mark(PunctuationMark), { Sentence = sentence([Clause], PunctuationMark) }.
sentence(Sentence) --> first_independent_clause(Clause1), next_independent_clause(Clause2), punctuation_mark(PunctuationMark), { Sentence = sentence([Clause1, Clause2], PunctuationMark) }.

clause(Clause) --> subject(Subject), predicate(Predicate), { Clause = clause(independent, Subject, Predicate)}.
clause(Clause) --> coordinating_conjunction(Conjunction), subject(Subject), predicate(Predicate), { Clause = clause(independent, Conjunction, Subject, Predicate)}.
clause(Clause) --> subordinating_conjunction(Conjunction), subject(Subject), predicate(Predicate), { Clause = clause(dependent, Conjunction, Subject, Predicate)}.
first_independent_clause(Clause) --> clause(Clause), { Clause = clause(independent, _, _) }.
next_independent_clause(Clause) --> clause(Clause), { Clause = clause(independent, _, _, _) }.
dependent_clause(Clause) --> clause(Clause), { Clause = clause(dependent, _, _, _) }.

subject(Subject) --> noun_phrase(NounPhrase), { Subject = subject(NounPhrase) }.

predicate(Predicate) --> verb_phrase(VerbPhrase), { Predicate = predicate(intransitive, VerbPhrase) }.
predicate(Predicate) --> verb_phrase(VerbPhrase), noun_phrase(NounPhrase), { Predicate = predicate(monotransitive, VerbPhrase, direct_object(NounPhrase)) }.
predicate(Predicate) --> verb_phrase(VerbPhrase), noun_phrase(IndirectObject), noun_phrase(DirectObject), { Predicate = predicate(ditransitive, VerbPhrase, indirect_object(IndirectObject), direct_object(DirectObject)) }.

noun_phrase(NounPhrase) --> pronoun(NounPhrase) ; noun(NounPhrase).
noun_phrase(NounPhrase) --> adjective_phrase(AttributiveAdjective), noun(Noun), { NounPhrase = noun_phrase(attributive_adjective(AttributiveAdjective), Noun) }.
noun_phrase(NounPhrase) --> noun(Noun), adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Noun, postpositive_adjective(PostpositiveAdjective)) }.
noun_phrase(NounPhrase) --> adjective_phrase(AttributiveAdjective), noun(Noun), adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(attributive_adjective(AttributiveAdjective), Noun, postpositive_adjective(PostpositiveAdjective)) }.

noun_phrase(NounPhrase) --> determiner(Determiner), noun(Noun), { NounPhrase = noun_phrase(Determiner, Noun) }.
noun_phrase(NounPhrase) --> determiner(Determiner), adjective_phrase(AttributiveAdjective), noun(Noun), { NounPhrase = noun_phrase(Determiner, attributive_adjective(AttributiveAdjective), Noun) }.
noun_phrase(NounPhrase) --> determiner(Determiner), noun(Noun), adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Determiner, Noun, postpositive_adjective(PostpositiveAdjective)) }.
noun_phrase(NounPhrase) --> determiner(Determiner), adjective_phrase(AttributiveAdjective), noun(Noun), adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Determiner, attributive_adjective(AttributiveAdjective), Noun, postpositive_adjective(PostpositiveAdjective)) }.

noun_phrase(NounPhrase) --> noun_phrase(Phrase1), coordinating_conjunction(Conjunction), noun_phrase(Phrase2), { NounPhrase = noun_phrase(conjunction(Conjunction, Phrase1, Phrase2)) }.


verb_phrase(VerbPhrase) --> present_simple(VerbPhrase).


adjective_phrase(AdjectivePhrase) --> adjective(AdjectivePhrase) ; cardinal(AdjectivePhrase) ; past_participle(AdjectivePhrase).
adjective_phrase(AdjectivePhrase) --> adverb_phrase(Adverb), adjective_phrase(Adjective), { AdjectivePhrase = adjective_phrase(Adverb, Adjective) }.
adjective_phrase(AdjectivePhrase) --> adjective_phrase(Phrase1), coordinating_conjunction(Conjunction), adjective_phrase(Phrase2), { AdjectivePhrase = adjective_phrase(conjunction(Conjunction, Phrase1, Phrase2)) }.

adverb_phrase(AdverbPhrase) --> adverb(AdverbPhrase).
adverb_phrase(AdverbPhrase) --> adverb_phrase(Adverb1), adverb_phrase(Adverb2), { AdverbPhrase = adverb_phrase(Adverb1, Adverb2) }.
adverb_phrase(AdverbPhrase) --> adverb_phrase(Phrase1), coordinating_conjunction(Conjunction), adverb_phrase(Phrase2), { AdverbPhrase = adverb_phrase(conjunction(Conjunction, Phrase1, Phrase2)) }.





% determines(Determiner, Noun, det(Determiner, Noun)).
% modifies(Modifier, Modifyee, iz(Modifyee, Modifier)).
% concatenates(Conjunction, Phrase1, Phrase2, ResultTerm) :- ResultTerm =.. [Conjunction, Phrase1, Phrase2].
% infinitive(InfinitiveMarker, Verb, inf(InfinitiveMarker, Verb)).

verb(verb(present_simple, read)) --> [read].
verb(verb(present_simple, write)) --> [write].
verb(verb(present_simple, is)) --> ['is'].
verb(verb(linking, is)) --> ['is'].

present_simple(Verb) --> verb(Verb), { Verb = verb(present_simple, _) }.


determiner(the) --> [the].

pronoun(he) --> [he].

noun(john) --> [john].
noun(annie) --> [annie].

past_participle(colored) --> [colored].

adjective(green) --> [green].

adverb(clearly) --> [clearly].
adverb(very) --> [very].

cardinal(10) --> [10].

conjunction(conjunction(coordinating, and)) --> [and].
coordinating_conjunction(Conjunction) --> conjunction(Conjunction), { Conjunction = conjunction(coordinating, _) }.
subordinating_conjunction(Conjunction) --> conjunction(Conjunction), { Conjunction = conjunction(subordinating, _) }.


infinitive_marker(to) --> [to].

punctuation_mark(punctuation_mark(period, '.')) --> ['.'].


