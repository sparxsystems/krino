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

                declarativeClause.AddTransition("init", GrammarAttributes.Subject);
                declarativeClause.AddTransition(GrammarAttributes.Subject, GrammarAttributes.Predicate);
                declarativeClause.AddTransition(GrammarAttributes.Predicate, "final");

                root.AddTransition("init", GrammarAttributes.Clause.Declarative)
                    .AddTransition(GrammarAttributes.Clause.Declarative, "final");
            }

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;

        private void AddNounElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.O.Free.Pronoun, GrammarAttributes.Morpheme.U.Free.Conjunction);

            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.Attributive, recursion);
            AddAdjectiveElement(objectBuilder, GrammarAttributes.AdjectiveElement.PostPositive, recursion);
            AddInfinitivePhrase(objectBuilder, GrammarAttributes.InfinitivePhrase, recursion);

            objectBuilder.AddTransition("init", GrammarAttributes.AdjectiveElement.Attributive)
                .AddTransition("init", GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.Morpheme.A.Free.Determiner)
                .AddTransition("init", GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.O.Free.Noun)
                .AddTransition("init", GrammarAttributes.Morpheme.O.Free.Pronoun, GrammarAttributes.Morpheme.O.Free.Pronoun)
                .AddTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.AdjectiveElement.Attributive, GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.O.Free.Noun)

                .AddTransition(GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.AdjectiveElement.Attributive)
                .AddTransition(GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.O.Free.Noun)

                .AddTransition(GrammarAttributes.Morpheme.O.Free.Pronoun, GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.Morpheme.O.Free.Pronoun, "final")

                .AddTransition(GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.InfinitivePhrase)
                .AddTransition(GrammarAttributes.Morpheme.O.Free.Noun, "final")

                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.O.Free.Noun, GrammarAttributes.Morpheme.O.Free.Noun)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.AdjectiveElement.Attributive)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.A.Free.Determiner, GrammarAttributes.Morpheme.A.Free.Determiner)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.O.Free.Pronoun, GrammarAttributes.Morpheme.O.Free.Pronoun)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final")

                .AddTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveBuilder = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.A, GrammarAttributes.Morpheme.U);

            AddAdverbElement(adjectiveBuilder, GrammarAttributes.AdverbElement, recursion);
            AddInfinitivePhrase(adjectiveBuilder, GrammarAttributes.InfinitivePhrase, recursion);

            adjectiveBuilder.AddTransition("init", "A", GrammarAttributes.Morpheme.A.Free.Adjective)
                .AddTransition("init", GrammarAttributes.AdverbElement)
                .AddTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.AdverbElement, "A")
                
                .AddTransition("A", GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition("A", "final")
                
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, "A", GrammarAttributes.Morpheme.A.Free.Adjective)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.AdverbElement)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbElement = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.E, GrammarAttributes.Morpheme.U);

            AddInfinitivePhrase(adverbElement, GrammarAttributes.InfinitivePhrase, recursion);

            adverbElement.AddTransition("init", GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddTransition("init", GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.Morpheme.E.Free.Adverb, "final")

                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.InfinitivePhrase)

                .AddTransition(GrammarAttributes.InfinitivePhrase, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddPredicateElement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var predicate = builder.AddSubState(objectType);

            predicate.AddSubState(GrammarAttributes.VerbElement)
                .AddStates(GrammarAttributes.Morpheme.I, GrammarAttributes.Morpheme.E, GrammarAttributes.Morpheme.U)
                .AddTransition("init", GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.E.Free.Adverb)
                .AddTransition(GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating)
                .AddTransition(GrammarAttributes.Morpheme.I.Free.Verb, "final")
                .AddTransition(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb)
                .AddTransition(GrammarAttributes.Morpheme.U.Free.Conjunction.Coordinating, GrammarAttributes.Morpheme.I.Free.Verb, GrammarAttributes.Morpheme.I.Free.Verb);

            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Indirect, recursion);
            AddNounElement(predicate, GrammarAttributes.Object.ObjectOfVerb.Direct, recursion);

            AddObjectComplement(predicate, GrammarAttributes.Complement.ObjectComplement, recursion);
            AddAdverbialComplement(predicate, GrammarAttributes.Complement.AdverbialComplement, recursion);
            AddSubjectComplement(predicate, GrammarAttributes.Complement.SubjectComplement, recursion);
            AddAdjectiveComplement(predicate, GrammarAttributes.Complement.AdjectiveComplement, recursion);
            
            //AddNounElement(predicate, GrammarAttributes.Complement.AdverbialComplement, GrammarAttributes.Morpheme.E.Free.Adverb | GrammarAttributes.Morpheme.E.Free.Preposition);

            predicate.AddTransition("init", GrammarAttributes.VerbElement);

            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Trivalent, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Quadrivalent, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Pentavalent);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Morpheme.I.Free.Verb.Valency.Bivalent);
            predicate.AddTransition(GrammarAttributes.VerbElement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransitionWithPreviousWordRule(GrammarAttributes.VerbElement, GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Morpheme.I.Free.Verb.Stative.Linking);
            predicate.AddTransition(GrammarAttributes.VerbElement, "final");

            predicate.AddTransition(GrammarAttributes.Object.ObjectOfVerb.Indirect, GrammarAttributes.Object.ObjectOfVerb.Direct);

            predicate.AddTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.ObjectComplement);
            predicate.AddTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransition(GrammarAttributes.Object.ObjectOfVerb.Direct, "final");

            predicate.AddTransition(GrammarAttributes.Complement.ObjectComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransition(GrammarAttributes.Complement.ObjectComplement, "final");

            predicate.AddTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransition(GrammarAttributes.Complement.SubjectComplement, GrammarAttributes.Complement.AdjectiveComplement);
            predicate.AddTransition(GrammarAttributes.Complement.SubjectComplement, "final");

            predicate.AddTransition(GrammarAttributes.Complement.AdjectiveComplement, GrammarAttributes.Complement.AdverbialComplement);
            predicate.AddTransition(GrammarAttributes.Complement.AdjectiveComplement, "final");

            predicate.AddTransition(GrammarAttributes.Complement.AdverbialComplement, "final");
        }


        private void AddSubjectComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var subjectComplement = builder.AddSubState(objectType);

            AddNounElement(subjectComplement, GrammarAttributes.NounElement, recursion);
            AddAdjectiveElement(subjectComplement, GrammarAttributes.AdjectiveElement.Predicative, recursion);

            subjectComplement.AddTransition("init", GrammarAttributes.NounElement);
            subjectComplement.AddTransition("init", GrammarAttributes.AdjectiveElement.Predicative);
            subjectComplement.AddTransition(GrammarAttributes.NounElement, "final");
            subjectComplement.AddTransition(GrammarAttributes.AdjectiveElement.Predicative, "final");
        }

        private void AddObjectComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectComplement = builder.AddSubState(objectType);

            AddNounElement(objectComplement, GrammarAttributes.NounElement, recursion);
            AddAdjectiveElement(objectComplement, GrammarAttributes.AdjectiveElement.PostPositive, recursion);

            objectComplement.AddTransition("init", GrammarAttributes.NounElement);
            objectComplement.AddTransition("init", GrammarAttributes.AdjectiveElement.PostPositive);
            objectComplement.AddTransition(GrammarAttributes.NounElement, "final");
            objectComplement.AddTransition(GrammarAttributes.AdjectiveElement.PostPositive, "final");
        }


        private void AddAdverbialComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.PrepositionalPhrase, recursion);
            AddAdverbElement(adverbialComplement, GrammarAttributes.AdverbElement, recursion);

            adverbialComplement.AddTransition("init", GrammarAttributes.PrepositionalPhrase);
            adverbialComplement.AddTransition("init", GrammarAttributes.AdverbElement);
            adverbialComplement.AddTransition(GrammarAttributes.PrepositionalPhrase, "final");
            adverbialComplement.AddTransition(GrammarAttributes.AdverbElement, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.PrepositionalPhrase, recursion);
            AddPrepositionalPhrase(adverbialComplement, GrammarAttributes.InfinitivePhrase, recursion);

            adverbialComplement.AddTransition("init", GrammarAttributes.PrepositionalPhrase);
            adverbialComplement.AddTransition("init", GrammarAttributes.InfinitivePhrase);

            adverbialComplement.AddTransition(GrammarAttributes.PrepositionalPhrase, "final");

            adverbialComplement.AddTransition(GrammarAttributes.InfinitivePhrase, "final");
        }

        private void AddPrepositionalPhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.E);

            AddNounElement(objectOfPreposition, GrammarAttributes.Object.ObjectOfPreposition, recursion);

            objectOfPreposition.AddTransition("init", GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Morpheme.E.Free.Preposition);
            objectOfPreposition.AddTransition(GrammarAttributes.Morpheme.E.Free.Adverb, GrammarAttributes.Object.ObjectOfPreposition);
            objectOfPreposition.AddTransition(GrammarAttributes.Object.ObjectOfPreposition, "final");
        }

        private void AddInfinitivePhrase(GrammarMachineBuilder builder, EnumBase objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddStates(GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker);

            AddPredicateElement(objectOfPreposition, GrammarAttributes.Predicate, recursion);

            objectOfPreposition.AddTransition("init", "to", GrammarAttributes.Morpheme.I.Free.Verb.InfinitiveMarker);
            objectOfPreposition.AddTransition("to", GrammarAttributes.Predicate);
            objectOfPreposition.AddTransition(GrammarAttributes.Predicate, "final");
        }


    }
}
