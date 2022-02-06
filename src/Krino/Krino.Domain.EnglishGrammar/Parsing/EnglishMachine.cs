using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;

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

            var declarativeClause = root.AddSubState(GrammarAttributes.Clause.Declarative);
            if (declarativeClause != null)
            {
                AddNounElement(declarativeClause, GrammarAttributes.Subject, myMaxRecursion);
                AddPredicateElement(declarativeClause, GrammarAttributes.Predicate, myMaxRecursion);

                declarativeClause.AddEmptyTransition("init", GrammarAttributes.Subject);
                declarativeClause.AddEmptyTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);
                declarativeClause.AddEmptyTransition(GrammarAttributes.Predicate, "final");

                root.AddEmptyTransition("init", GrammarAttributes.Clause.Declarative)
                    .AddEmptyTransition(GrammarAttributes.Clause.Declarative, "final");
            }

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;

        private void AddNounElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.A.Free.Determiner,
                           GrammarAttributes.Morpheme.O.Free.Noun,
                           GrammarAttributes.Morpheme.O.Free.Pronoun,
                           GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating);

            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.Attributive, recursion);
            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.PostPositive, recursion);
            AddInfinitivePhrase(objectBuilder, GrammarAttributes.InfinitivePhrase, recursion);

            objectBuilder.AddEmptyTransition("init", GrammarAttributes.AdjectiveElement.Attributive)
                .AddTriggeredTransition("init", GrammarAttributes.Morpheme.A.Free.Determiner)
                .AddTriggeredTransition("init", GrammarAttributes.Morpheme.O.Free.Noun)
                .AddTriggeredTransition("init", GrammarAttributes.Morpheme.O.Free.Pronoun)
                .AddEmptyTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.Morpheme.O.Free.Noun)

                .AddEmptyTransition(GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.AdjectiveElement.Attributive)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.Morpheme.O.Free.Noun)

                .AddTriggeredTransition(GrammarAttributes.Morpheme.O.Free.Pronoun, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.O.Free.Pronoun, "final")

                .AddTriggeredTransition(GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.InfinitivePhrase)
                .AddEmptyTransition(GrammarAttributes.Morpheme.O.Free.Noun, "final")

                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.O.Free.Noun)
                .AddEmptyTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.AdjectiveElement.Attributive)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.A.Free.Determiner)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.O.Free.Pronoun)
                .AddEmptyTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.InfinitivePhrase)

                .AddEmptyTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final")

                .AddTriggeredTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.A.Free.Adjective, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating);

            AddAdverbElement(adjectiveBuilder, GrammarAttributes.AdverbElement, recursion);
            AddInfinitivePhrase(adjectiveBuilder, GrammarAttributes.InfinitivePhrase, recursion);

            adjectiveBuilder.AddTriggeredTransition("init", GrammarAttributes.Morpheme.A.Free.Adjective)
                .AddEmptyTransition("init", GrammarAttributes.AdverbElement)
                .AddEmptyTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.AdverbElement, GrammarAttributes.Morpheme.A.Free.Adjective)
                
                .AddTriggeredTransition(GrammarAttributes.Morpheme.A.Free.Adjective, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.A.Free.Adjective, "final")
                
                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.A.Free.Adjective)
                .AddEmptyTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.AdverbElement)
                .AddEmptyTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbElement = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating);

            AddInfinitivePhrase(adverbElement, GrammarAttributes.InfinitivePhrase, recursion);

            adverbElement.AddTriggeredTransition("init", GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddEmptyTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.E.Free.Adverb, "final")

                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddEmptyTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.InfinitivePhrase)

                .AddTriggeredTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddPredicateElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var predicate = builder.AddSubState(objectType);

            predicate.AddSubState(GrammarAttributes.VerbElement)
                .AddStates(GrammarAttributes.Morpheme.I.Free.Verb,
                           GrammarAttributes.Morpheme.E.Free.Adverb,
                           GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)

                .AddTriggeredTransition("init", GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddEmptyTransition(GrammarAttributes.Morpheme.I.Free.Verb, "final")
                .AddTriggeredTransition(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTriggeredTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.I.Free.Verb);

            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Indirect, recursion);
            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Direct, recursion);

            AddObjectComplement(predicate, GrammarAttributes.Complement.ObjectComplement, recursion);
            AddAdverbialComplement(predicate, GrammarAttributes.Complement.AdverbialComplement, recursion);
            AddSubjectComplement(predicate, GrammarAttributes.Complement.SubjectComplement, recursion);
            AddAdjectiveComplement(predicate, GrammarAttributes.Complement.AdjectiveComplement, recursion);
            
            //AddNounElement(predicate, GrammarAttributes.Complement.AdverbialComplement, GrammarAttributes.Morpheme.E.Free.Adverb | GrammarAttributes.Morpheme.E.Free.Preposition);

            predicate.AddEmptyTransition("init", GrammarAttributes.VerbElement);

            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Trivalent, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Quadrivalent, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Pentavalent);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent);
            predicate.AddEmptyTransition(GrammarAttributes.VerbElement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Morpheme.I.Free.Verb.Stative.Linking);
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

            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.PrepositionalPhrase, recursion);
            AddAdverbElement(adverbialComplement, GrammarAttributes.AdverbElement, recursion);

            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.PrepositionalPhrase);
            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.AdverbElement);
            adverbialComplement.AddEmptyTransition(GrammarAttributes.PrepositionalPhrase, "final");
            adverbialComplement.AddEmptyTransition(GrammarAttributes.AdverbElement, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.PrepositionalPhrase, recursion);
            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.InfinitivePhrase, recursion);

            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.PrepositionalPhrase);
            adverbialComplement.AddEmptyTransition("init", GrammarAttributes.InfinitivePhrase);

            adverbialComplement.AddEmptyTransition(GrammarAttributes.PrepositionalPhrase, "final");

            adverbialComplement.AddEmptyTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddPrepositionalPhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.E.Free.Preposition);

            AddNounElement(objectOfPreposition, GrammarAttributes.Object.ObjectOfPreposition, recursion);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.E.Free.Preposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.E.Free.Preposition, GrammarAttributes.Object.ObjectOfPreposition);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Object.ObjectOfPreposition, "final");
        }

        private void AddInfinitivePhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker);

            AddPredicateElement(objectOfPreposition, GrammarAttributes.Predicate, recursion);

            objectOfPreposition.AddTriggeredTransition("init", GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker, GrammarAttributes.Predicate);
            objectOfPreposition.AddEmptyTransition(GrammarAttributes.Predicate, "final");
        }


    }
}
