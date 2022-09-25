:- module(krino_english_grammar,
    [
       
    ]).

% Eliminate the left recusion infinite loop.
:- table sentence/3.
:- table clauze/3.
:- table subject/3.
:- table predicate/3.
:- table noun_phrase/3.
:- table adjective_phrase/3.
:- table adverb_phrase/3.


text(Text) --> sentences(Sentences), { Text = text(Sentences) }.

sentence(Sentence) --> independent_clauses(Clauses), punctuation_mark(PunctuationMark), { Sentence = sentence(Clauses, PunctuationMark) }.
sentences([Sentence]) --> sentence(Sentence).
sentences(Sentences) --> sentence(Sentence), sentences(NextSentences), { append([Sentence], NextSentences, Sentences) }.

clauze(Clause) --> subject(Subject), predicate(Predicate), { Clause = clauze(independent, Subject, Predicate)}.
clauze(Clause) --> coordinating_conjunction(Conjunction), subject(Subject), predicate(Predicate), { Clause = clauze(independent, Conjunction, Subject, Predicate)}.
clauze(Clause) --> subordinating_conjunction(Conjunction), subject(Subject), predicate(Predicate), { Clause = clauze(dependent, Conjunction, Subject, Predicate)}.

independent_clause(Clause) --> clauze(Clause), { Clause = clauze(independent, _, _, _) ;  Clause = clauze(independent, _, _) }.
dependent_clause(Clause) --> clauze(Clause), { Clause = clauze(dependent, _, _, _) }.

independent_clauses([Clause]) --> independent_clause(Clause).
independent_clauses(Clauses) --> independent_clause(Clause), independent_clauses(NextClauses), { append([Clause], NextClauses, Clauses) }.


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

verb_phrase(VerbPhrase) --> present_simple(Verb), { VerbPhrase = verb_phrase(Verb) }.

present_simple(PresentSimple) --> present_verb(Verb), { PresentSimple = present_simple(Verb) }.
present_simple(PresentSimple) --> verb(AuxVerb), present_verb(Verb),
    { 
        (AuxVerb = verb(auxiliary(primary(_))) ; AuxVerb = verb(auxiliary(modal(_)))),
        PresentSimple = present_simple(AuxVerb, Verb)
    }.


adjective_phrase(AdjectivePhrase) --> adjective(AdjectivePhrase) ; cardinal(AdjectivePhrase) ; past_participle(AdjectivePhrase).
adjective_phrase(AdjectivePhrase) --> adverb_phrase(Adverb), adjective_phrase(Adjective), { AdjectivePhrase = adjective_phrase(Adverb, Adjective) }.
adjective_phrase(AdjectivePhrase) --> adjective_phrase(Phrase1), coordinating_conjunction(Conjunction), adjective_phrase(Phrase2), { AdjectivePhrase = adjective_phrase(conjunction(Conjunction, Phrase1, Phrase2)) }.

adverb_phrase(AdverbPhrase) --> adverb(AdverbPhrase).
adverb_phrase(AdverbPhrase) --> adverb_phrase(Adverb1), adverb_phrase(Adverb2), { AdverbPhrase = adverb_phrase(Adverb1, Adverb2) }.
adverb_phrase(AdverbPhrase) --> adverb_phrase(Phrase1), coordinating_conjunction(Conjunction), adverb_phrase(Phrase2), { AdverbPhrase = adverb_phrase(conjunction(Conjunction, Phrase1, Phrase2)) }.

infinitive_phrase(InfinitivePhrase) --> infinitive_marker(To), present_verb(Verb), { InfinitivePhrase = infinitive_phrase(To, Verb) }.



% determines(Determiner, Noun, det(Determiner, Noun)).
% modifies(Modifier, Modifyee, iz(Modifyee, Modifier)).
% concatenates(Conjunction, Phrase1, Phrase2, ResultTerm) :- ResultTerm =.. [Conjunction, Phrase1, Phrase2].
% infinitive(InfinitiveMarker, Verb, inf(InfinitiveMarker, Verb)).

present_verb(Verb) --> verb(Verb), { Verb = verb(present(_)) }.


regular_verb(Verb, Base, Past) :- Verb = verb(regular(Base)) ; all_verb_forms(Verb,  Base,  Base, Base, Base, Base, Base, Base,  Past, Past, Past, Past, Past, Past,  Past).
irregular_tense_verb(Verb, Base, Past, PastParticple) :- Verb = verb(irregular(Base)) ; all_verb_forms(Verb,  Base,  Base, Base, Base, Base, Base, Base,  Past, Past, Past, Past, Past, Past,  PastParticple).

all_verb_forms(VerbTerm, Base,
               PresentSingularFirstPerson, PresentSingularSecondPerson, PresentSingularThirdPerson,
               PresentPluralFirstPerson, PresentPluralSecondPerson, PresentPluralThirdPerson,
               PastSingularFirstPerson, PastSingularSecondPerson, PastSingularThirdPerson,
               PastPluralFirstPerson, PastPluralSecondPerson, PastPluralThirdPerson,
               PastParticiple) :-
    VerbTerm = verb(base(Base)) ;

    VerbTerm = verb(present(singular(first_person(PresentSingularFirstPerson)))) ;
    VerbTerm = verb(present(singular(second_person(PresentSingularSecondPerson)))) ;
    VerbTerm = verb(present(singular(third_person(PresentSingularThirdPerson)))) ;
    VerbTerm = verb(present(plural(first_person(PresentPluralFirstPerson)))) ;
    VerbTerm = verb(present(plural(second_person(PresentPluralSecondPerson)))) ;
    VerbTerm = verb(present(plural(third_person(PresentPluralThirdPerson)))) ;

    VerbTerm = verb(past(singular(first_person(PastSingularFirstPerson)))) ;
    VerbTerm = verb(past(singular(second_person(PastSingularSecondPerson)))) ;
    VerbTerm = verb(past(singular(third_person(PastSingularThirdPerson)))) ;
    VerbTerm = verb(past(plural(first_person(PastPluralFirstPerson)))) ;
    VerbTerm = verb(past(plural(second_person(PastPluralSecondPerson)))) ;
    VerbTerm = verb(past(plural(third_person(PastPluralThirdPerson)))) ;

    VerbTerm = verb(past_participle(PastParticiple)).
    




verb(Verb) --> ['be'], { Verb = verb(irregular(be)) ; all_verb_forms(Verb, be,  am, are, 'is', are, are, are,  was, were, was, were, were, were,  been) }.
verb(verb(linking('be'))) --> ['be'].

verb(Verb) --> [do], { irregular_tense_verb(Verb, do, did, done) }.
verb(verb(auxiliary(primary(do)))) --> [do].

verb(verb(present_simple, read)) --> [read].
verb(verb(present_simple, write)) --> [write].




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


