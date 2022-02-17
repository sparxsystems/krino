using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Parsing
{
    public class EnglishMachine
    {
        private int myMaxRecursion = 10;
        private MultiMachine<LinguisticState, IWord> myMachine;

        public EnglishMachine()
        {
            myMachine = new MultiMachine<LinguisticState, IWord>();

            var root = new GrammarMachineBuilder(myMachine);

            AddSimpleSentence(root, GrammarAttributes.Sentence.Simple, myMaxRecursion);

            root.AddEmptyTransition("init", GrammarAttributes.Sentence.Simple);
            root.AddEmptyTransition(GrammarAttributes.Sentence.Simple, "final");

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;


        private void AddSimpleSentence(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var simpleSentence = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.PunctuationMark);

            AddDeclarativeClause(simpleSentence, GrammarAttributes.Clause.Declarative, recursion);

            simpleSentence.AddEmptyTransition("init", GrammarAttributes.Clause.Declarative);
            simpleSentence.AddTriggeredTransition(GrammarAttributes.Clause.Declarative, GrammarAttributes.PunctuationMark);
            simpleSentence.AddEmptyTransition(GrammarAttributes.PunctuationMark, "final");
        }


        private void AddDeclarativeClause(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var declarativeClause = builder.AddSubState(objectType);

            AddNounElement(declarativeClause, GrammarAttributes.Subject, recursion);
            AddVerbElement(declarativeClause, GrammarAttributes.Predicate, recursion);

            declarativeClause.AddEmptyTransition("init", GrammarAttributes.Subject);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);
            declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");
        }


        private void AddNounElement(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var nounElement = builder.AddSubState(attributes);

            AddNounPhrase(nounElement, GrammarAttributes.Phrase.NounPhrase, recursion);

            nounElement.AddEmptyTransition("init", GrammarAttributes.Phrase.NounPhrase);

            nounElement.AddEmptyTransition(GrammarAttributes.Phrase.NounPhrase, "final");
        }


        private void AddVerbElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var predicate = builder.AddSubState(objectType);

            AddVerbPhrase(predicate, GrammarAttributes.VerbElement, recursion);

            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Indirect, recursion);
            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Direct, recursion);

            AddObjectComplement(predicate, GrammarAttributes.Complement.ObjectComplement, recursion);
            AddAdverbialComplement(predicate, GrammarAttributes.Complement.AdverbialComplement, recursion);
            AddSubjectComplement(predicate, GrammarAttributes.Complement.SubjectComplement, recursion);
            AddAdjectiveComplement(predicate, GrammarAttributes.Complement.AdjectiveComplement, recursion);
            AddAdverbElement(predicate, GrammarAttributes.Adverbial, recursion);


            predicate.AddEmptyTransition("init", GrammarAttributes.VerbElement);

            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Trivalent, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Quadrivalent, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Pentavalent);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Morpheme.Free.Lexical.Verb.Valency.Bivalent);
            predicate.AddEmptyTransition(GrammarAttributes.VerbElement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Morpheme.Free.Lexical.Verb.Stative.Linking);
            predicate.AddEmptyTransition(GrammarAttributes.VerbElement, GrammarAttributes.Adverbial);
            predicate.AddEmptyTransition(GrammarAttributes.VerbElement, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Object.ObjectOfVerb.Direct);

            predicate.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.ObjectComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Complement.ObjectComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Complement.ObjectComplement, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Complement.AdjectiveComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Complement.SubjectComplement, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Complement.AdjectiveComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddEmptyTransition(GrammarAttributes.Complement.AdjectiveComplement, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Complement.AdverbialComplement, "final");

            predicate.AddEmptyTransition(GrammarAttributes.Adverbial, "final");
        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adjective, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            AddAdverbElement(adjectiveBuilder, GrammarAttributes.AdverbElement, recursion);
            AddInfinitivePhrase(adjectiveBuilder, GrammarAttributes.Phrase.InfinitivePhrase, recursion);

            adjectiveBuilder.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddEmptyTransition("init", GrammarAttributes.AdverbElement)
                .AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.AdverbElement, GrammarAttributes.Morpheme.Free.Lexical.Adjective)

                .AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Adjective, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adjective, "final")

                .AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Morpheme.Free.Lexical.Adjective)
                .AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.AdverbElement)
                .AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Phrase.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.Phrase.InfinitivePhrase, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbElement = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Adverb, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            AddInfinitivePhrase(adverbElement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);

            adverbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Adverb, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Adverb, "final")

                .AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Morpheme.Free.Lexical.Adverb)
                .AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Phrase.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.Phrase.InfinitivePhrase, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
        }



        private void AddNounPhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.Free.Functional.Determiner,
                           GrammarAttributes.Morpheme.Free.Lexical.Noun,
                           GrammarAttributes.Morpheme.Free.Functional.Pronoun,
                           GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);

            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.Attributive, recursion);
            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.PostPositive, recursion);

            objectBuilder.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Attributive);
            objectBuilder.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Determiner);
            objectBuilder.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Noun);
            objectBuilder.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            objectBuilder.AddTriggeredTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.Morpheme.Free.Lexical.Noun);

            objectBuilder.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.AdjectiveElement.Attributive);
            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Determiner, GrammarAttributes.Morpheme.Free.Lexical.Noun);

            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Pronoun, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            objectBuilder.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Pronoun, "final");

            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating);
            objectBuilder.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Noun, "final");

            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Morpheme.Free.Lexical.Noun);
            objectBuilder.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.AdjectiveElement.Attributive);
            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Morpheme.Free.Functional.Determiner);
            objectBuilder.AddTriggeredTransition(GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating, GrammarAttributes.Morpheme.Free.Functional.Pronoun);

            objectBuilder.AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }

        private void AddVerbPhrase(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var verbElement = builder.AddSubState(attributes);

            AddPresentSimple(verbElement, GrammarAttributes.VerbElement.Sememe.PresentSimpleTense, recursion);
            //var presentContinuousState = AddPresentContinuous(verbElement, recursion);
            //var presentPerfectState = AddPresentPerfect(verbElement, recursion);

            verbElement.AddEmptyTransition("init", GrammarAttributes.VerbElement.Sememe.PresentSimpleTense);
            //verbElement.AddEmptyTransition("init", presentContinuousState);
            //verbElement.AddEmptyTransition("init", presentPerfectState);

            verbElement.AddEmptyTransition(GrammarAttributes.VerbElement.Sememe.PresentSimpleTense, "final");
            //verbElement.AddEmptyTransition(presentContinuousState, "final");
            //verbElement.AddEmptyTransition(presentPerfectState, "final");
        }


        private void AddPresentSimple(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var verbElement = builder.AddSubState(attributes)
                .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentFirstPersonSingular,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentSecondPersonSingular,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular,
                           GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentAnyPersonPlural);

            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base);
            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentFirstPersonSingular);
            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentSecondPersonSingular);
            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular);
            verbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentAnyPersonPlural);

            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Base, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentFirstPersonSingular, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentSecondPersonSingular, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentThirdPersonSingular, "final");
            verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PresentAnyPersonPlural, "final");
        }

        //private void AddPresentContinuous(GrammarMachineBuilder builder, BigInteger attributes, int recursion)
        //{
        //    using var _t = Trace.Entering();

        //    if (--recursion == 0) return;

        //    var verbElement = builder.AddSubState(attributes)
        //        .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);

        //    verbElement.AddTriggeredTransition("Init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing);
        //    verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");
        //}

        //private BigInteger AddPresentPerfect(GrammarMachineBuilder builder, int recursion)
        //{
        //    using var _t = Trace.Entering();

        //    if (--recursion == 0) return 0;

        //    var verbElement = builder.AddSubState(GrammarAttributes.VerbElement.Sememe.Time.Present | GrammarAttributes.VerbElement.Sememe.Aspect.Perfect)
        //        .AddStates(GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary, GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.PastParticiple);

        //    verbElement.AddTriggeredTransition("Init", GrammarAttributes.Morpheme.Free.Lexical.Verb.Auxiliary, ParsingRule.AuxiliaryWordIs("have", "has"));
        //    verbElement.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.Form.Ing, "final");

        //    return verbElement.ParentState.Attributes;
        //}


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

            AddVerbElement(objectOfPreposition, GrammarAttributes.Predicate, recursion);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.Free.Lexical.Verb.InfinitiveMarker, GrammarAttributes.Predicate);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Predicate, "final");
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


        private void AddAdverbialComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.Phrase.PrepositionalPhrase, recursion);
            AddAdverbElement(adverbialComplement, GrammarAttributes.AdverbElement, recursion);

            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.AdverbElement);
            adverbialComplement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");
            adverbialComplement.AddEmptyTransition(GrammarAttributes.AdverbElement, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adjectiveComplement, GrammarAttributes.Phrase.PrepositionalPhrase, recursion);
            AddPrepositionalPhrase(adjectiveComplement, GrammarAttributes.Phrase.InfinitivePhrase, recursion);

            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.PrepositionalPhrase);
            adjectiveComplement.AddEmptyTransition("init", GrammarAttributes.Phrase.InfinitivePhrase);

            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.PrepositionalPhrase, "final");

            adjectiveComplement.AddEmptyTransition(GrammarAttributes.Phrase.InfinitivePhrase, "final");
        }

    }
}
