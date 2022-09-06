:- module(krino_english_grammar,
    [
       
    ]).

% Eliminate the left recusion infinite loop.
:- table noun_phrase/3.
:- table adjective_phrase/3.
:- table adverb_phrase/3.


clause(VerbPhrase) --> noun_phrase(Subject), verb_phrase(Subject, VerbPhrase).


noun_phrase(NounPhrase) --> pronoun(NounPhrase) ; noun(NounPhrase).
noun_phrase(NounPhrase) --> attributive_adjective_phrase(AttrAdjective), noun(Noun), { modifies(AttrAdjective, Noun, NounPhrase) }.
noun_phrase(NounPhrase) --> noun(Noun), postpositive_adjective_phrase(PostAdjective), { modifies(PostAdjective, Noun, NounPhrase) }.
noun_phrase(NounPhrase) --> attributive_adjective_phrase(AttrAdjective), noun(Noun), postpositive_adjective_phrase(PostAdjective), { modifies(AttrAdjective, Noun, M1), modifies(PostAdjective, M1, NounPhrase) }.

noun_phrase(NounPhrase) --> determiner(Determiner), noun(Noun), { determines(Determiner, Noun, NounPhrase) }.
noun_phrase(NounPhrase) --> determiner(Determiner), attributive_adjective_phrase(AttrAdjective), noun(Noun), { modifies(AttrAdjective, Noun, M), determines(Determiner, M, NounPhrase) }.
noun_phrase(NounPhrase) --> determiner(Determiner), noun(Noun), postpositive_adjective_phrase(PostAdjective), { modifies(PostAdjective, Noun, M), determines(Determiner, M, NounPhrase) }.
noun_phrase(NounPhrase) --> determiner(Determiner), attributive_adjective_phrase(AttrAdjective), noun(Noun), postpositive_adjective_phrase(PostAdjective), { modifies(AttrAdjective, Noun, M1), modifies(PostAdjective, M1, M2), determines(Determiner, M2, NounPhrase) }.


attributive_adjective_phrase(AdjectivePhrase) --> adjective_phrase(AdjectivePhrase).
postpositive_adjective_phrase(AdjectivePhrase) --> adjective_phrase(AdjectivePhrase).

adjective_phrase(AdjectivePhrase) --> adjective(AdjectivePhrase) ; cardinal(AdjectivePhrase) ; past_participle(AdjectivePhrase).
adjective_phrase(AdjectivePhrase) --> adverb_phrase(AdverbModifier), adjective_phrase(Adjective), { modifies(AdverbModifier, Adjective, AdjectivePhrase) }.

adverb_phrase(AdverbPhrase) --> adverb(AdverbPhrase).
adverb_phrase(AdverbPhrase) --> adverb_phrase(Adverb1), adverb_phrase(Adverb2), { modifies(Adverb1, Adverb2, AdverbPhrase) }.
adverb_phrase(AdverbPhrase) --> adverb_phrase(Phrase1), coordinating_conjunction(Conjunction), adverb_phrase(Phrase2), { concatenates(Conjunction, Phrase1, Phrase2, AdverbPhrase) }.


verb_phrase(Subject, VerbPhrase) --> intransitive_verb(Subject, VerbPhrase).
verb_phrase(Subject, VerbPhrase) --> monotransitive_verb(Subject, Y, VerbPhrase), noun_phrase(Y).

determines(Determiner, Noun, det(Determiner, Noun)).
modifies(Modifier, Modifyee, iz(Modifyee, Modifier)).
concatenates(Conjunction, Phrase1, Phrase2, ResultTerm) :- ResultTerm =.. [Conjunction, Phrase1, Phrase2].

intransitive_verb(Subject, paints(Subject)) --> [paints].
monotransitive_verb(Subject, Y, likes(Subject, Y)) --> [likes].

determiner(the) --> [the].

pronoun(he) --> [he].

noun(john) --> [john].
noun(annie) --> [annie].

past_participle(colored) --> [colored].

adjective(green) --> [green].

adverb(clearly) --> [clearly].
adverb(very) --> [very].

cardinal(10) --> [10].

coordinating_conjunction(and) --> [and].


