﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Parsing
{
    public class EnglishMachine
    {
        private MultiMachine<LinguisticState, IWord> myMachine;
        private Dictionary<string, int> myRestrictions = new Dictionary<string, int>();


        public MultiMachine<LinguisticState, IWord> Machine => myMachine;


        public EnglishMachine()
            : this(false)
        {
        }

        public EnglishMachine(bool simpleMode)
        {
            if (simpleMode)
            {
                AddRestriction(nameof(AddSingleDependentClause));
            }

            myMachine = new MultiMachine<LinguisticState, IWord>();

            var root = new GrammarMachineBuilder(myMachine);

            AddText(root, GrammarAttributes.Text);

            root.AddEmptyTransition("init", GrammarAttributes.Text);
            root.AddEmptyTransition(GrammarAttributes.Text, "final");

            myMachine.Reset();
        }

        
        private void AddText(GrammarMachineBuilder builder, EnumBase objectType)
        {
            var text = builder.AddSubState(objectType);
            text.AddState("start_loop", 0);
            text.AddState("end_loop", 0);

            AddSimpleSentence(text, GrammarAttributes.Sentence.Simple);
            AddCompoundSentence(text, GrammarAttributes.Sentence.Compound, GrammarAttributes.Clause.Independent);

            text.AddEmptyTransition("init", "start_loop");

            text.AddEmptyTransition("start_loop", GrammarAttributes.Sentence.Simple);
            text.AddEmptyTransition("start_loop", GrammarAttributes.Sentence.Compound);

            text.AddEmptyTransition(GrammarAttributes.Sentence.Simple, "end_loop");
            
            text.AddEmptyTransition(GrammarAttributes.Sentence.Compound, "end_loop");

            // If a next sentence.
            text.AddEmptyTransition("end_loop", "start_loop");

            // If no other sentence.
            text.AddEmptyTransition("end_loop", "final");

        }


        private void AddSimpleSentence(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            var simpleSentence = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.PunctuationMark);

            AddSingleIndependentClause(simpleSentence, GrammarAttributes.Clause.Independent);

            simpleSentence.AddEmptyTransition("init", GrammarAttributes.Clause.Independent);
            simpleSentence.AddTriggeredTransition(GrammarAttributes.Clause.Independent, GrammarAttributes.PunctuationMark);
            simpleSentence.AddEmptyTransition(GrammarAttributes.PunctuationMark, "final");
        }

        private void AddCompoundSentence(GrammarMachineBuilder builder, EnumBase groupAttributes, EnumBase itemAttributes)
            => AddMandatoryConcatenation(builder, groupAttributes, itemAttributes, AddSingleIndependentClause);

        private void AddSingleIndependentClause(GrammarMachineBuilder builder, BigInteger objectType)
        {
            using var _t = Trace.Entering();

            var declarativeClause = builder.AddSubState(objectType);

            AddNounElement(declarativeClause, GrammarAttributes.Subject);
            AddVerbElement(declarativeClause, GrammarAttributes.Predicate, false);

            declarativeClause.AddEmptyTransition("init", GrammarAttributes.Subject);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");
        }

        private void AddSingleDependentClause(GrammarMachineBuilder builder, BigInteger objectType)
        {
            using var _t = Trace.Entering();

            if (myRestrictions.ContainsKey(nameof(AddSingleDependentClause)))
            {
                return;
            }

            AddRestriction(nameof(AddSingleDependentClause));

            var declarativeClause = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);

            AddNounElement(declarativeClause, GrammarAttributes.Subject);
            AddVerbElement(declarativeClause, GrammarAttributes.Predicate, false);

            declarativeClause.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);

            declarativeClause.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating, GrammarAttributes.Subject);
            
            declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);

            declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");

            RemoveRestriction(nameof(AddSingleDependentClause));
        }


        private void AddNounElement(GrammarMachineBuilder builder, BigInteger attributes) => AddConcatenation(builder, attributes, AddSingleNounElement);
        private void AddSingleNounElement(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var nounElement = builder.AddSubState(attributes);

            AddNounPhrase(nounElement, GrammarAttributes.Phrase.NounPhrase);
            AddInfinitivePhrase(nounElement, GrammarAttributes.Phrase.InfinitivePhrase);
            AddSingleDependentClause(nounElement, GrammarAttributes.Clause.Dependent.NounClause);

            nounElement.AddEmptyTransition("init", GrammarAttributes.Phrase.NounPhrase);
            nounElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            nounElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.NounClause);

            nounElement.AddEmptyTransition(GrammarAttributes.Phrase.NounPhrase, "final");

            nounElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");

            nounElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.NounClause, "final");
        }

        private void AddVerbElement(GrammarMachineBuilder builder, EnumBase objectType, bool onlyPresentSimple)
            => AddConcatenation(builder, objectType, (builder, attributes) => AddSingleVerbElement(builder, attributes, onlyPresentSimple));
        private void AddSingleVerbElement(GrammarMachineBuilder builder, BigInteger attributes, bool onlyPresentSimple)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes);

            AddVerbPhrase(verbElement, GrammarAttributes.Phrase.VerbPhrase, onlyPresentSimple);

            AddNounElement(verbElement, GrammarAttributes.Object.ObjectOfVerb.Indirect);
            AddNounElement(verbElement, GrammarAttributes.Object.ObjectOfVerb.Direct);

            AddObjectComplement(verbElement, GrammarAttributes.Complement.ObjectComplement);
            AddAdverbElement(verbElement, GrammarAttributes.Complement.AdverbialComplement);
            AddSubjectComplement(verbElement, GrammarAttributes.Complement.SubjectComplement);
            AddAdjectiveComplement(verbElement, GrammarAttributes.Complement.AdjectiveComplement);
            AddAdverbElement(verbElement, GrammarAttributes.AdverbialAdjunct);


            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase);

            verbElement.AddEmptyTransitionWithVerbValencyRule(GrammarAttributes.Phrase.VerbPhrase, GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Quadrivalent, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Pentavalent);
            verbElement.AddEmptyTransitionWithVerbValencyRule(GrammarAttributes.Phrase.VerbPhrase, GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            verbElement.AddEmptyTransitionWithVerbValencyRule(GrammarAttributes.Phrase.VerbPhrase, GrammarAttributes.Complement.AdverbialComplement, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent);
            verbElement.AddEmptyTransitionWithVerbValencyRule(GrammarAttributes.Phrase.VerbPhrase, GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking);
            verbElement.AddEmptyTransitionWithVerbValencyRule(GrammarAttributes.Phrase.VerbPhrase, GrammarAttributes.AdverbialAdjunct, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Monovalent);
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Object.ObjectOfVerb.Direct);

            verbElement.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.ObjectComplement);
            verbElement.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.AdverbialComplement);
            verbElement.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.AdverbialAdjunct);
            verbElement.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Complement.ObjectComplement, GrammarAttributes.AdverbialAdjunct);
            verbElement.AddEmptyTransition(GrammarAttributes.Complement.ObjectComplement, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.AdverbialAdjunct);
            verbElement.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Complement.AdjectiveComplement);
            verbElement.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Complement.AdjectiveComplement, GrammarAttributes.AdverbialAdjunct);
            verbElement.AddEmptyTransition(GrammarAttributes.Complement.AdjectiveComplement, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Complement.AdverbialComplement, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.AdverbialAdjunct, "final");
        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, BigInteger objectType) => AddConcatenation(builder, objectType, AddSingleAdjectiveElement);
        private void AddSingleAdjectiveElement(GrammarMachineBuilder builder, BigInteger objectType)
        {
            using var _t = Trace.Entering();

            var adjectiveElement = builder.AddSubState(objectType);

            // If a noun can act as an adjective.
            if (GrammarAttributes.AdjectiveElement.Attributive.IsIn(objectType))
            {
                adjectiveElement.AddStates(GrammarAttributes.Morpheme.Free.Lexical.Noun);
            }

            AddAdjectivePhrase(adjectiveElement, GrammarAttributes.Phrase.AdjectivePhrase);
            AddInfinitivePhrase(adjectiveElement, GrammarAttributes.Phrase.InfinitivePhrase);

            // If the adjectiv is postpositive then it can also be an adjective clause or adjectival prepositional phrase.
            if (GrammarAttributes.AdjectiveElement.PostPositive.IsIn(objectType))
            {
                AddSingleDependentClause(adjectiveElement, GrammarAttributes.Clause.Dependent.AdjectiveClause);
                AddPrepositionalPhrase(adjectiveElement, GrammarAttributes.Phrase.PrepositionalPhrase);
            }

            adjectiveElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Phrase.AdjectivePhrase);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.AdjectiveClause);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Phrase.AdjectivePhrase, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.AdjectiveClause, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, BigInteger objectType) => AddConcatenation(builder, objectType, AddSingleAdverbElement);
        private void AddSingleAdverbElement(GrammarMachineBuilder builder, BigInteger objectType)
        {
            using var _t = Trace.Entering();

            var adverbElement = builder.AddSubState(objectType);
            adverbElement.AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adverb);

            AddPrepositionalPhrase(adverbElement, GrammarAttributes.Phrase.PrepositionalPhrase);
            AddInfinitivePhrase(adverbElement, GrammarAttributes.Phrase.InfinitivePhrase);
            AddSingleDependentClause(adverbElement, GrammarAttributes.Clause.Dependent.AdverbialClause);

            adverbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adverb);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.AdverbialClause);

            adverbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adverb, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.AdverbialClause, "final");
        }





        private void AddNounPhrase(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            var nounPhrase = builder.AddSubState(objectType);
            nounPhrase.AddStates(GrammarAttributes.Morpheme.Free.Functional.Determiner,
                                    GrammarAttributes.Morpheme.Free.Lexical.Noun,
                                    GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            AddAdjectiveElement(nounPhrase, GrammarAttributes.AdjectiveElement.Attributive);
            AddAdjectiveElement(nounPhrase, GrammarAttributes.AdjectiveElement.PostPositive);

            nounPhrase.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Attributive);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            nounPhrase.AddTriggeredTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.Morpheme.Free.Lexical.Noun);
            nounPhrase.AddEmptyTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.AdjectiveElement.Attributive);

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.AdjectiveElement.Attributive);
            nounPhrase.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.Morpheme.Free.Lexical.Noun);

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Pronoun, "final");

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, GrammarAttributes.AdjectiveElement.PostPositive);
            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, "final");

            nounPhrase.AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }

        private void AddAdjectivePhrase(GrammarMachineBuilder builder, BigInteger objectType)
        {
            using var _t = Trace.Entering();

            var adjectivePhrase = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adjective,
                           GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal);

            AddAdverbElement(adjectivePhrase, GrammarAttributes.AdverbElement);

            adjectivePhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            adjectivePhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal);
            adjectivePhrase.AddEmptyTransition("init", GrammarAttributes.AdverbElement);

            adjectivePhrase.AddTriggeredTransition(GrammarAttributes.AdverbElement, GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            adjectivePhrase.AddTriggeredTransition(GrammarAttributes.AdverbElement, GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal);

            adjectivePhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adjective, "final");
            adjectivePhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Numeral.Cardinal, "final");
        }

        private void AddVerbPhrase(GrammarMachineBuilder builder, BigInteger attributes, bool onlyPresentSimple)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes);

            AddPresentSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense);

            if (!onlyPresentSimple)
            {
                AddPastSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastSimpleTense);
                AddPastContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousTense);
                AddPastPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastPerfectTense);
                AddPastContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousPerfectTense);

                AddPresentContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousTense);
                AddPresentPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentPerfectTense);
                AddPresentContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousPerfectTense);

                AddFutureSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureSimpleTense);
                AddFutureContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousTense);
                AddFuturePerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FuturePerfectTense);
                AddFutureContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousPerfectTense);
            }


            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PastSimpleTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PastPerfectTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousPerfectTense);

            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentPerfectTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousPerfectTense);

            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureSimpleTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.FuturePerfectTense);
            verbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousPerfectTense);

            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastSimpleTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastPerfectTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousPerfectTense, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentPerfectTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousPerfectTense, "final");

            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureSimpleTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.FuturePerfectTense, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousPerfectTense, "final");
        }


        private void AddPastSimple(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past, "final");
        }

        private void AddPastContinuous(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, ParsingRule.WordIsOneOf("was", "were"));

            verbElement.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddPastPerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddTriggeredTransition("init", "had", ParsingRule.WordIsOneOf("had"));

            verbElement.AddTriggeredTransition("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple, "final");
        }

        private void AddPastContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing)
                .AddState("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary);

            verbElement.AddTriggeredTransition("init", "had", ParsingRule.WordIsOneOf("had"));

            verbElement.AddTriggeredTransition("had", "been", ParsingRule.WordIs("been"));

            verbElement.AddTriggeredTransition("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddPresentSimple(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("do/does", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("init", "do/does", ParsingRule.WordIsOneOf("do", "does"));
            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("do/does", "not", ParsingRule.WordIsOneOf("not"));
            verbElement.AddTriggeredTransition("do/does", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("not", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base, "final");
        }

        private void AddPresentContinuous(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("am/are/is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", "am/are/is", ParsingRule.WordIsOneOf("am", "are", "is"));

            verbElement.AddTriggeredTransition("am/are/is", "not", ParsingRule.WordIsOneOf("not"));
            verbElement.AddTriggeredTransition("am/are/is", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("not", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddPresentPerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddTriggeredTransition("init", "have/has", ParsingRule.WordIsOneOf("have", "has"));
            
            verbElement.AddTriggeredTransition("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);
            verbElement.AddTriggeredTransition("have/has", "not", ParsingRule.WordIsOneOf("not"));

            verbElement.AddTriggeredTransition("not", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple, "final");
        }

        private void AddPresentContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation)
                .AddState("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", "have/has", ParsingRule.WordIsOneOf("have", "has"));

            verbElement.AddTriggeredTransition("have/has", "been", ParsingRule.WordIs("been"));
            verbElement.AddTriggeredTransition("have/has", "not", ParsingRule.WordIs("not"));

            verbElement.AddTriggeredTransition("not", "been", ParsingRule.WordIs("been"));

            verbElement.AddTriggeredTransition("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }


        private void AddFutureSimple(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal)
                .AddState("not", GrammarAttributes.Morpheme.Free.Lexical.Adverb.Negation)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("init", "will", ParsingRule.WordIsOneOf("will"));

            verbElement.AddTriggeredTransition("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            verbElement.AddTriggeredTransition("will", "not", ParsingRule.WordIsOneOf("not"));

            verbElement.AddTriggeredTransition("not", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base, "final");
        }

        private void AddFutureContinuous(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal)
                .AddState("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", "will", ParsingRule.WordIsOneOf("will"));

            verbElement.AddTriggeredTransition("will", "be", ParsingRule.WordIsOneOf("be"));

            verbElement.AddTriggeredTransition("be", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddFuturePerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal)
                .AddState("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddTriggeredTransition("init", "will", ParsingRule.WordIsOneOf("will"));

            verbElement.AddTriggeredTransition("will", "have", ParsingRule.WordIsOneOf("have"));

            verbElement.AddTriggeredTransition("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple, "final");
        }

        private void AddFutureContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal)
                .AddState("have", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", "will", ParsingRule.WordIsOneOf("will"));

            verbElement.AddTriggeredTransition("will", "have", ParsingRule.WordIsOneOf("have"));

            verbElement.AddTriggeredTransition("have", "been", ParsingRule.WordIsOneOf("been"));

            verbElement.AddTriggeredTransition("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }


        private void AddPrepositionalPhrase(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            if (myRestrictions.ContainsKey(nameof(AddPrepositionalPhrase)))
            {
                return;
            }

            AddRestriction(nameof(AddInfinitivePhrase));
            AddRestriction(nameof(AddPrepositionalPhrase));

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Functional.Preposition);

            AddNounElement(objectOfPreposition, GrammarAttributes.Object.ObjectOfPreposition);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Preposition, GrammarAttributes.Object.ObjectOfPreposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Object.ObjectOfPreposition, "final");

            RemoveRestriction(nameof(AddInfinitivePhrase));
            RemoveRestriction(nameof(AddPrepositionalPhrase));
        }

        private void AddInfinitivePhrase(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            if (myRestrictions.ContainsKey(nameof(AddInfinitivePhrase)))
            {
                return;
            }

            AddRestriction(nameof(AddInfinitivePhrase));

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);

            AddSingleVerbElement(objectOfPreposition, GrammarAttributes.VerbElement, true);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker, GrammarAttributes.VerbElement);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.VerbElement, "final");

            RemoveRestriction(nameof(AddInfinitivePhrase));
        }



        private void AddSubjectComplement(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            var subjectComplement = builder.AddSubState(objectType);

            AddNounElement(subjectComplement, GrammarAttributes.NounElement);
            AddAdjectiveElement(subjectComplement, GrammarAttributes.AdjectiveElement.Predicative);

            subjectComplement.AddEmptyTransition("init", GrammarAttributes.NounElement);
            subjectComplement.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Predicative);
            subjectComplement.AddEmptyTransition(GrammarAttributes.NounElement, "final");
            subjectComplement.AddEmptyTransition(GrammarAttributes.AdjectiveElement.Predicative, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            var adjectiveComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adjectiveComplement, GrammarAttributes.Phrase.PrepositionalPhrase);
            AddInfinitivePhrase(adjectiveComplement, GrammarAttributes.Phrase.InfinitivePhrase);
            AddSingleDependentClause(adjectiveComplement, GrammarAttributes.Clause.Dependent.NounClause);

            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.NounClause);

            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");
            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.NounClause, "final");
        }

        private void AddObjectComplement(GrammarMachineBuilder builder, EnumBase objectType)
        {
            using var _t = Trace.Entering();

            var objectComplement = builder.AddSubState(objectType);

            AddNounElement(objectComplement, GrammarAttributes.NounElement);
            AddAdjectiveElement(objectComplement, GrammarAttributes.AdjectiveElement.PostPositive);

            objectComplement.AddEmptyTransition("init", GrammarAttributes.NounElement);
            objectComplement.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.PostPositive);
            objectComplement.AddEmptyTransition(GrammarAttributes.NounElement, "final");
            objectComplement.AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }


        private void AddConcatenation(GrammarMachineBuilder builder, BigInteger objectType, Action<GrammarMachineBuilder, BigInteger> addSingleElement)
        {
            using var _t = Trace.Entering();

            var concatenatingState = builder.AddSubState(objectType);
            concatenatingState.AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            var concatItem = objectType | GrammarAttributes.Item;
            addSingleElement(concatenatingState, concatItem);

            concatenatingState.AddEmptyTransition("init", concatItem);

            concatenatingState.AddTriggeredTransition(concatItem, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            concatenatingState.AddEmptyTransition(concatItem, "final");

            concatenatingState.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, concatItem);
        }

        private void AddMandatoryConcatenation(GrammarMachineBuilder builder, BigInteger groupObject, BigInteger itemObject, Action<GrammarMachineBuilder, BigInteger> addSingleElement)
        {
            using var _t = Trace.Entering();

            var concatenatingState = builder.AddSubState(groupObject);
            concatenatingState.AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            addSingleElement(concatenatingState, itemObject);

            concatenatingState.AddEmptyTransition("init", itemObject);

            concatenatingState.AddTriggeredTransition(itemObject, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            concatenatingState.AddEmptyTransitionWithRules(itemObject.GetGrammarId(), "final", ParsingRule.IsConcatenated());

            concatenatingState.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, itemObject);
        }



        public void AddRestriction(string restrictionTag)
        {
            if (!myRestrictions.ContainsKey(restrictionTag))
            {
                myRestrictions[restrictionTag] = 1;
            }
            else
            {
                ++myRestrictions[restrictionTag];
            }
        }

        public void RemoveRestriction(string restrictionTag)
        {
            if (myRestrictions.TryGetValue(restrictionTag, out var counter))
            {
                --counter;

                if (counter > 0)
                {
                    myRestrictions[restrictionTag] = counter;
                }
                else
                {
                    myRestrictions.Remove(restrictionTag);
                }
            }
        }
    }
}
