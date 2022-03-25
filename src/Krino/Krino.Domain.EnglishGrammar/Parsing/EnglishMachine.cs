using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Parsing
{
    public class EnglishMachine
    {
        private int myMaxRecursion = 10;
        private MultiMachine<LinguisticState, IWord> myMachine;


        public MultiMachine<LinguisticState, IWord> Machine => myMachine;


        public EnglishMachine()
        {
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

            AddSimpleSentence(text, GrammarAttributes.Sentence.Simple, myMaxRecursion);
            AddCompoundSentence(text, GrammarAttributes.Sentence.Compound, GrammarAttributes.Clause.Independent, myMaxRecursion);

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


        private void AddSimpleSentence(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var simpleSentence = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.PunctuationMark);

            AddSingleIndependentClause(simpleSentence, GrammarAttributes.Clause.Independent, recursion);

            simpleSentence.AddEmptyTransition("init", GrammarAttributes.Clause.Independent);
            simpleSentence.AddTriggeredTransition(GrammarAttributes.Clause.Independent, GrammarAttributes.PunctuationMark);
            simpleSentence.AddEmptyTransition(GrammarAttributes.PunctuationMark, "final");
        }

        private void AddCompoundSentence(GrammarMachineBuilder builder, EnumBase groupAttributes, EnumBase itemAttributes, int recursion)
            => AddMandatoryConcatenation(builder, groupAttributes, itemAttributes, recursion, AddSingleIndependentClause);

        private void AddSingleIndependentClause(GrammarMachineBuilder builder, BigInteger objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var declarativeClause = builder.AddSubState(objectType);

            AddNounElement(declarativeClause, GrammarAttributes.Subject, recursion);
            AddVerbElement(declarativeClause, GrammarAttributes.Predicate, 0, recursion);

            declarativeClause.AddEmptyTransition("init", GrammarAttributes.Subject);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");
        }

        private void AddSingleDependentClause(GrammarMachineBuilder builder, BigInteger objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var declarativeClause = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);

            AddNounElement(declarativeClause, GrammarAttributes.Subject, recursion);
            AddVerbElement(declarativeClause, GrammarAttributes.Predicate, 0, recursion);

            declarativeClause.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating);

            declarativeClause.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Subordinating, GrammarAttributes.Subject);
            
            declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);

            declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");
        }


        private void AddNounElement(GrammarMachineBuilder builder, BigInteger attributes, int recursion) => AddConcatenation(builder, attributes, recursion, AddSingleNounElement);
        private void AddSingleNounElement(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var nounElement = builder.AddSubState(attributes);

            AddNounPhrase(nounElement, GrammarAttributes.Phrase.NounPhrase, recursion);
            AddInfinitivePhrase(nounElement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);
            AddSingleDependentClause(nounElement, GrammarAttributes.Clause.Dependent.NounClause, recursion);

            nounElement.AddEmptyTransition("init", GrammarAttributes.Phrase.NounPhrase);
            nounElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            nounElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.NounClause);

            nounElement.AddEmptyTransition(GrammarAttributes.Phrase.NounPhrase, "final");

            nounElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");

            nounElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.NounClause, "final");
        }

        private void AddVerbElement(GrammarMachineBuilder builder, EnumBase objectType, BigInteger verbPhraseFilter, int recursion)
            => AddConcatenation(builder, objectType, recursion, (builder, attributes, recursion) => AddSingleVerbElement(builder, attributes, verbPhraseFilter, recursion));
        private void AddSingleVerbElement(GrammarMachineBuilder builder, BigInteger attributes, BigInteger verbPhraseFilter, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var verbElement = builder.AddSubState(attributes);

            AddVerbPhrase(verbElement, GrammarAttributes.Phrase.VerbPhrase, verbPhraseFilter, recursion);

            AddNounElement(verbElement, GrammarAttributes.Object.ObjectOfVerb.Indirect, recursion);
            AddNounElement(verbElement, GrammarAttributes.Object.ObjectOfVerb.Direct, recursion);

            AddObjectComplement(verbElement, GrammarAttributes.Complement.ObjectComplement, recursion);
            AddAdverbElement(verbElement, GrammarAttributes.Complement.AdverbialComplement, recursion);
            AddSubjectComplement(verbElement, GrammarAttributes.Complement.SubjectComplement, recursion);
            AddAdjectiveComplement(verbElement, GrammarAttributes.Complement.AdjectiveComplement, recursion);
            AddAdverbElement(verbElement, GrammarAttributes.AdverbialAdjunct, recursion);


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

        private void AddAdjectiveElement(GrammarMachineBuilder builder, BigInteger objectType, int recursion) => AddConcatenation(builder, objectType, recursion, AddSingleAdjectiveElement);
        private void AddSingleAdjectiveElement(GrammarMachineBuilder builder, BigInteger objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveElement = builder.AddSubState(objectType);

            // If a noun can act as an adjective.
            if (GrammarAttributes.AdjectiveElement.Attributive.IsIn(objectType))
            {
                adjectiveElement.AddStates(GrammarAttributes.Morpheme.Free.Lexical.Noun);
            }

            AddAdjectivePhrase(adjectiveElement, GrammarAttributes.Phrase.AdjectivePhrase, recursion);
            AddInfinitivePhrase(adjectiveElement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);

            // If the adjectiv is postpositive then it can also be the adjective clause.
            if (GrammarAttributes.AdjectiveElement.PostPositive.IsIn(objectType))
            {
                AddSingleDependentClause(adjectiveElement, GrammarAttributes.Clause.Dependent.AdjectiveClause, recursion);
            }

            adjectiveElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Phrase.AdjectivePhrase);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            adjectiveElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.AdjectiveClause);

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Phrase.AdjectivePhrase, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");

            adjectiveElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.AdjectiveClause, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, BigInteger objectType, int recursion) => AddConcatenation(builder, objectType, recursion, AddSingleAdverbElement);
        private void AddSingleAdverbElement(GrammarMachineBuilder builder, BigInteger objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbElement = builder.AddSubState(objectType);
            adverbElement.AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adverb);

            AddPrepositionalPhrase(adverbElement, GrammarAttributes.Phrase.PrepositionalPhrase, recursion);
            AddInfinitivePhrase(adverbElement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);
            AddSingleDependentClause(adverbElement, GrammarAttributes.Clause.Dependent.AdverbialClause, recursion);

            adverbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adverb);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);
            adverbElement.AddEmptyTransition("init", GrammarAttributes.Clause.Dependent.AdverbialClause);

            adverbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adverb, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
            adverbElement.AddEmptyTransition(GrammarAttributes.Clause.Dependent.AdverbialClause, "final");
        }





        private void AddNounPhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var nounPhrase = builder.AddSubState(objectType);
            nounPhrase.AddStates(GrammarAttributes.Morpheme.Free.Functional.Determiner,
                                    GrammarAttributes.Morpheme.Free.Lexical.Noun,
                                    GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            AddAdjectiveElement(nounPhrase, GrammarAttributes.AdjectiveElement.Attributive, recursion);
            AddAdjectiveElement(nounPhrase, GrammarAttributes.AdjectiveElement.PostPositive, recursion);

            nounPhrase.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Attributive);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            nounPhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            nounPhrase.AddTriggeredTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.Morpheme.Free.Lexical.Noun);
            nounPhrase.AddEmptyTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.AdjectiveElement.Attributive);

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.AdjectiveElement.Attributive);
            nounPhrase.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.Morpheme.Free.Lexical.Noun);

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Pronoun, "final");

            nounPhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, "final");

            nounPhrase.AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }

        private void AddAdjectivePhrase(GrammarMachineBuilder builder, BigInteger objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectivePhrase = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adjective);

            AddAdverbElement(adjectivePhrase, GrammarAttributes.AdverbElement, recursion);

            adjectivePhrase.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adjective);
            adjectivePhrase.AddEmptyTransition("init", GrammarAttributes.AdverbElement);

            adjectivePhrase.AddTriggeredTransition(GrammarAttributes.AdverbElement, GrammarAttributes.Morpheme.Free.Lexical.Adjective);

            adjectivePhrase.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adjective, "final");
        }

        private void AddVerbPhrase(GrammarMachineBuilder builder, BigInteger attributes, BigInteger verbPhraseFilter, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var verbElement = builder.AddSubState(attributes);


            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastSimpleTense, verbPhraseFilter))
            {
                AddPastSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastSimpleTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousTense, verbPhraseFilter))
            {
                AddPastContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastPerfectTense, verbPhraseFilter))
            {
                AddPastPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastPerfectTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousPerfectTense, verbPhraseFilter))
            {
                AddPastContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PastContinuousPerfectTense, recursion);
            }



            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense, verbPhraseFilter))
            {
                AddPresentSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousTense, verbPhraseFilter))
            {
                AddPresentContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentPerfectTense, verbPhraseFilter))
            {
                AddPresentPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentPerfectTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousPerfectTense, verbPhraseFilter))
            {
                AddPresentContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentContinuousPerfectTense, recursion);
            }



            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureSimpleTense, verbPhraseFilter))
            {
                AddFutureSimple(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureSimpleTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousTense, verbPhraseFilter))
            {
                AddFutureContinuous(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.FuturePerfectTense, verbPhraseFilter))
            {
                AddFuturePerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FuturePerfectTense, recursion);
            }

            if (verbPhraseFilter == 0 || EnumBase.IsIn(GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousPerfectTense, verbPhraseFilter))
            {
                AddFutureContinuousPerfect(verbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.FutureContinuousPerfectTense, recursion);
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


        private void AddPastSimple(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Past, "final");
        }

        private void AddPastContinuous(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, ParsingRule.WordIsOneOf("was", "were"));

            verbElement.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddPastPerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddTriggeredTransition("init", "had", ParsingRule.WordIsOneOf("had"));

            verbElement.AddTriggeredTransition("had", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple, "final");
        }

        private void AddPastContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
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

        private void AddPresentSimple(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base, "final");
        }

        private void AddPresentContinuous(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, ParsingRule.WordIsOneOf("am", "are", "is"));

            verbElement.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }

        private void AddPresentPerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

            verbElement.AddTriggeredTransition("init", "have/has", ParsingRule.WordIsOneOf("have", "has"));
            
            verbElement.AddTriggeredTransition("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);
            
            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple, "final");
        }

        private void AddPresentContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("have/has", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddState("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Primary)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddTriggeredTransition("init", "have/has", ParsingRule.WordIsOneOf("have", "has"));

            verbElement.AddTriggeredTransition("have/has", "been", ParsingRule.WordIs("been"));

            verbElement.AddTriggeredTransition("been", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        }


        private void AddFutureSimple(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            var verbElement = builder.AddSubState(attributes)
                .AddState("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary.Modal)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddTriggeredTransition("init", "will", ParsingRule.WordIsOneOf("will"));
            verbElement.AddTriggeredTransition("will", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base, "final");
        }

        private void AddFutureContinuous(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
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

        private void AddFuturePerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
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

        private void AddFutureContinuousPerfect(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
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


        private void AddPrepositionalPhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Functional.Preposition);

            AddNounElement(objectOfPreposition, GrammarAttributes.Object.ObjectOfPreposition, recursion);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Preposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Preposition, GrammarAttributes.Object.ObjectOfPreposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Object.ObjectOfPreposition, "final");
        }

        private void AddInfinitivePhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);

            AddSingleVerbElement(objectOfPreposition, GrammarAttributes.VerbElement, GrammarAttributes.Phrase.VerbPhrase.Sememe.PresentSimpleTense, recursion);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker, GrammarAttributes.VerbElement);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.VerbElement, "final");
        }



        private void AddSubjectComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var subjectComplement = builder.AddSubState(objectType);

            AddNounElement(subjectComplement, GrammarAttributes.NounElement, recursion);
            AddAdjectiveElement(subjectComplement, GrammarAttributes.AdjectiveElement.Predicative, recursion);

            subjectComplement.AddEmptyTransition("init", GrammarAttributes.NounElement);
            subjectComplement.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Predicative);
            subjectComplement.AddEmptyTransition(GrammarAttributes.NounElement, "final");
            subjectComplement.AddEmptyTransition(GrammarAttributes.AdjectiveElement.Predicative, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adjectiveComplement, GrammarAttributes.Phrase.PrepositionalPhrase, recursion);
            AddInfinitivePhrase(adjectiveComplement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);

            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);

            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");

            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
        }

        private void AddObjectComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectComplement = builder.AddSubState(objectType);

            AddNounElement(objectComplement, GrammarAttributes.NounElement, recursion);
            AddAdjectiveElement(objectComplement, GrammarAttributes.AdjectiveElement.PostPositive, recursion);

            objectComplement.AddEmptyTransition("init", GrammarAttributes.NounElement);
            objectComplement.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.PostPositive);
            objectComplement.AddEmptyTransition(GrammarAttributes.NounElement, "final");
            objectComplement.AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }


        


        private void AddConcatenation(GrammarMachineBuilder builder, BigInteger objectType, int recursion, Action<GrammarMachineBuilder, BigInteger, int> addSingleElement)
        {
            using var _t = Trace.Entering();

            // Do not modify the recursion.
            if (recursion == 0) return;

            var concatenatingState = builder.AddSubState(objectType);
            concatenatingState.AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            var concatItem = objectType | GrammarAttributes.Item;
            addSingleElement(concatenatingState, concatItem, recursion);

            concatenatingState.AddEmptyTransition("init", concatItem);

            concatenatingState.AddTriggeredTransition(concatItem, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            concatenatingState.AddEmptyTransition(concatItem, "final");

            concatenatingState.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, concatItem);
        }

        private void AddMandatoryConcatenation(GrammarMachineBuilder builder, BigInteger groupObject, BigInteger itemObject, int recursion, Action<GrammarMachineBuilder, BigInteger, int> addSingleElement)
        {
            using var _t = Trace.Entering();

            // Do not modify the recursion.
            if (recursion == 0) return;

            var concatenatingState = builder.AddSubState(groupObject);
            concatenatingState.AddStates(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            addSingleElement(concatenatingState, itemObject, recursion);

            concatenatingState.AddEmptyTransition("init", itemObject);

            concatenatingState.AddTriggeredTransition(itemObject, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            concatenatingState.AddEmptyTransitionWithRules(itemObject.GetGrammarId(), "final", ParsingRule.IsConcatenated());

            concatenatingState.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, itemObject);
        }
    }
}
