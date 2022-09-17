:- module(krino_english_grammar,
    [
       
    ]).

% Eliminate the left recusion infinite loop.
:- table k_clause/3.
:- table k_subject/3.
:- table k_predicate/3.
:- table k_noun_phrase/3.
:- table k_adjective_phrase/3.
:- table k_adverb_phrase/3.


k_clause(Clause) --> k_subject(Subject), k_predicate(Predicate), { Clause = clause(Subject, Predicate)}.

k_subject(Subject) --> k_noun_phrase(NounPhrase), { Subject = subject(NounPhrase) }.

k_predicate(Predicate) --> k_verb_phrase(VerbPhrase), { Predicate = predicate(intransitive, VerbPhrase) }.
k_predicate(Predicate) --> k_verb_phrase(VerbPhrase), k_noun_phrase(NounPhrase), { Predicate = predicate(monotransitive, VerbPhrase, direct_object(NounPhrase)) }.
k_predicate(Predicate) --> k_verb_phrase(VerbPhrase), k_noun_phrase(IndirectObject), k_noun_phrase(DirectObject), { Predicate = predicate(ditransitive, VerbPhrase, indirect_object(IndirectObject), direct_object(DirectObject)) }.

k_noun_phrase(NounPhrase) --> k_pronoun(NounPhrase) ; k_noun(NounPhrase).
k_noun_phrase(NounPhrase) --> k_adjective_phrase(AttributiveAdjective), k_noun(Noun), { NounPhrase = noun_phrase(attributive_adjective(AttributiveAdjective), Noun) }.
k_noun_phrase(NounPhrase) --> k_noun(Noun), k_adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Noun, postpositive_adjective(PostpositiveAdjective)) }.
k_noun_phrase(NounPhrase) --> k_adjective_phrase(AttributiveAdjective), k_noun(Noun), k_adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(attributive_adjective(AttributiveAdjective), Noun, postpositive_adjective(PostpositiveAdjective)) }.

k_noun_phrase(NounPhrase) --> k_determiner(Determiner), k_noun(Noun), { NounPhrase = noun_phrase(Determiner, Noun) }.
k_noun_phrase(NounPhrase) --> k_determiner(Determiner), k_adjective_phrase(AttributiveAdjective), k_noun(Noun), { NounPhrase = noun_phrase(Determiner, attributive_adjective(AttributiveAdjective), Noun) }.
k_noun_phrase(NounPhrase) --> k_determiner(Determiner), k_noun(Noun), k_adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Determiner, Noun, postpositive_adjective(PostpositiveAdjective)) }.
k_noun_phrase(NounPhrase) --> k_determiner(Determiner), k_adjective_phrase(AttributiveAdjective), k_noun(Noun), k_adjective_phrase(PostpositiveAdjective), { NounPhrase = noun_phrase(Determiner, attributive_adjective(AttributiveAdjective), Noun, postpositive_adjective(PostpositiveAdjective)) }.

k_noun_phrase(NounPhrase) --> k_noun_phrase(Phrase1), k_coordinating_conjunction(Conjunction), k_noun_phrase(Phrase2), { NounPhrase = noun_phrase(conjunction(coordinating, Conjunction, Phrase1, Phrase2)) }.


k_verb_phrase(VerbPhrase) --> k_verb(VerbPhrase), { VerbPhrase = verb(present_simple, _) }.


k_adjective_phrase(AdjectivePhrase) --> k_adjective(AdjectivePhrase) ; k_cardinal(AdjectivePhrase) ; k_past_participle(AdjectivePhrase).
k_adjective_phrase(AdjectivePhrase) --> k_adverb_phrase(Adverb), k_adjective_phrase(Adjective), { AdjectivePhrase = adjective_phrase(Adverb, Adjective) }.
k_adjective_phrase(AdjectivePhrase) --> k_adjective_phrase(Phrase1), k_coordinating_conjunction(Conjunction), k_adjective_phrase(Phrase2), { AdjectivePhrase = adjective_phrase(conjunction(coordinating, Conjunction, Phrase1, Phrase2)) }.

k_adverb_phrase(AdverbPhrase) --> k_adverb(AdverbPhrase).
k_adverb_phrase(AdverbPhrase) --> k_adverb_phrase(Adverb1), k_adverb_phrase(Adverb2), { AdverbPhrase = adverb_phrase(Adverb1, Adverb2) }.
k_adverb_phrase(AdverbPhrase) --> k_adverb_phrase(Phrase1), k_coordinating_conjunction(Conjunction), k_adverb_phrase(Phrase2), { AdverbPhrase = adverb_phrase(conjunction(coordinating, Conjunction, Phrase1, Phrase2)) }.





% determines(Determiner, Noun, det(Determiner, Noun)).
% modifies(Modifier, Modifyee, iz(Modifyee, Modifier)).
% concatenates(Conjunction, Phrase1, Phrase2, ResultTerm) :- ResultTerm =.. [Conjunction, Phrase1, Phrase2].
% infinitive(InfinitiveMarker, Verb, inf(InfinitiveMarker, Verb)).

k_verb(verb(present_simple, read)) --> [read].
k_verb(verb(present_simple, write)) --> [write].
k_verb(verb(present_simple, is)) --> ['is'].
k_verb(verb(linking, is)) --> ['is'].


k_determiner(the) --> [the].

k_pronoun(he) --> [he].

k_noun(john) --> [john].
k_noun(annie) --> [annie].

k_past_participle(colored) --> [colored].

k_adjective(green) --> [green].

k_adverb(clearly) --> [clearly].
k_adverb(very) --> [very].

k_cardinal(10) --> [10].

k_coordinating_conjunction(and) --> [and].

k_infinitive_marker(to) --> [to].


