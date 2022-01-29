using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

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

            var declarativeClause = root.AddSubState(LinguisticStructureType.DeclarativeClause);
            if (declarativeClause != null)
            {
                AddNounElement(declarativeClause, LinguisticStructureType.Subject, myMaxRecursion);
                AddPredicateElement(declarativeClause, LinguisticStructureType.Predicate, myMaxRecursion);

                declarativeClause.AddTransition("init", LinguisticStructureType.Subject);
                declarativeClause.AddTransition(LinguisticStructureType.Subject, LinguisticStructureType.Predicate);
                declarativeClause.AddTransition(LinguisticStructureType.Predicate, "final");


                root.AddTransition("init", LinguisticStructureType.DeclarativeClause)
                    .AddTransition(LinguisticStructureType.DeclarativeClause, "final");
            }

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;

        private void AddNounElement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectBuilder = builder.AddSubState(objectType)
                .AddLexemeStates("Determiner", "Noun", "Pronoun", "U");

            AddAdjectiveElement(objectBuilder, LinguisticStructureType.AttributiveAdjective, recursion);
            AddAdjectiveElement(objectBuilder, LinguisticStructureType.PredicativeAdjective, recursion);
            AddInfinitivePhrase(objectBuilder, LinguisticStructureType.InfinitivePhrase, recursion);

            objectBuilder.AddTransition("init", LinguisticStructureType.AttributiveAdjective)
                .AddTransition("init", "Determiner", EnglishAttributes.A.Lexeme.Determiner)
                .AddTransition("init", "Noun", EnglishAttributes.O.Lexeme.Noun)
                .AddTransition("init", "Pronoun", EnglishAttributes.O.Lexeme.Pronoun)
                .AddTransition("init", LinguisticStructureType.InfinitivePhrase)

                .AddTransition(LinguisticStructureType.AttributiveAdjective, "Noun", EnglishAttributes.O.Lexeme.Noun)

                .AddTransition("Determiner", LinguisticStructureType.AttributiveAdjective)
                .AddTransition("Determiner", "Noun", EnglishAttributes.O.Lexeme.Noun)

                .AddTransition("Pronoun", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("Pronoun", "final")

                .AddTransition("Noun", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("Noun", LinguisticStructureType.PredicativeAdjective)
                .AddTransition("Noun", "final")

                .AddTransition("U", "Noun", EnglishAttributes.O.Lexeme.Noun)
                .AddTransition("U", LinguisticStructureType.AttributiveAdjective)
                .AddTransition("U", "Determiner", EnglishAttributes.A.Lexeme.Determiner)
                .AddTransition("U", "Pronoun", EnglishAttributes.O.Lexeme.Pronoun)
                .AddTransition("U", LinguisticStructureType.InfinitivePhrase)

                .AddTransition(LinguisticStructureType.PredicativeAdjective, "final")

                .AddTransition(LinguisticStructureType.InfinitivePhrase, "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition(LinguisticStructureType.InfinitivePhrase, "final");
        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adjectiveBuilder = builder.AddSubState(objectType)
                .AddLexemeStates("A", "U");

            AddAdverbElement(adjectiveBuilder, LinguisticStructureType.AdverbElement, recursion);
            AddInfinitivePhrase(adjectiveBuilder, LinguisticStructureType.InfinitivePhrase, recursion);

            adjectiveBuilder.AddTransition("init", "A", EnglishAttributes.A.Lexeme.Adjective)
                .AddTransition("init", LinguisticStructureType.AdverbElement)
                .AddTransition("init", LinguisticStructureType.InfinitivePhrase)

                .AddTransition(LinguisticStructureType.AdverbElement, "A")
                
                .AddTransition("A", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("A", "final")
                
                .AddTransition("U", "A", EnglishAttributes.A.Lexeme.Adjective)
                .AddTransition("U", LinguisticStructureType.AdverbElement)
                .AddTransition("U", LinguisticStructureType.InfinitivePhrase)

                .AddTransition(LinguisticStructureType.InfinitivePhrase, "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition(LinguisticStructureType.InfinitivePhrase, "final");
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbElement = builder.AddSubState(objectType)
                .AddLexemeStates("E", "U");

            AddInfinitivePhrase(adverbElement, LinguisticStructureType.InfinitivePhrase, recursion);

            adverbElement.AddTransition("init", "E", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("init", LinguisticStructureType.InfinitivePhrase)

                .AddTransition("E", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("E", "final")

                .AddTransition("U", "E", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("U", LinguisticStructureType.InfinitivePhrase)

                .AddTransition(LinguisticStructureType.InfinitivePhrase, "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition(LinguisticStructureType.InfinitivePhrase, "final");
        }

        private void AddPredicateElement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var predicate = builder.AddSubState(objectType);

            predicate.AddSubState(LinguisticStructureType.Verb)
                .AddLexemeStates("I", "E", "U")
                .AddTransition("init", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("I", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("I", "E", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("I", "U", EnglishAttributes.U.Lexeme.Conjunction)
                .AddTransition("I", "final")
                .AddTransition("E", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("U", "I", EnglishAttributes.I.Lexeme.Verb);

            AddNounElement(predicate, LinguisticStructureType.IndirectObject, recursion);
            AddNounElement(predicate, LinguisticStructureType.DirectObject, recursion);

            AddObjectComplement(predicate, LinguisticStructureType.ObjectComplement, recursion);
            AddAdverbialComplement(predicate, LinguisticStructureType.AdverbialComplement, recursion);
            AddSubjectComplement(predicate, LinguisticStructureType.SubjectComplement, recursion);
            AddAdjectiveComplement(predicate, LinguisticStructureType.AdjectiveComplement, recursion);
            
            //AddNounElement(predicate, LinguisticStructureType.AdverbialComplement, EnglishAttributes.E.Lexeme.Adverb | EnglishAttributes.E.Lexeme.Preposition);

            predicate.AddTransition("init", LinguisticStructureType.Verb);

            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.IndirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent);
            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.DirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent);
            predicate.AddTransition(LinguisticStructureType.Verb, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.SubjectComplement, EnglishAttributes.I.Lexeme.Verb.Stative.Linking);
            predicate.AddTransition(LinguisticStructureType.Verb, "final");

            predicate.AddTransition(LinguisticStructureType.IndirectObject, LinguisticStructureType.DirectObject);

            predicate.AddTransition(LinguisticStructureType.DirectObject, LinguisticStructureType.ObjectComplement);
            predicate.AddTransition(LinguisticStructureType.DirectObject, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.DirectObject, "final");

            predicate.AddTransition(LinguisticStructureType.ObjectComplement, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.ObjectComplement, "final");

            predicate.AddTransition(LinguisticStructureType.SubjectComplement, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.SubjectComplement, LinguisticStructureType.AdjectiveComplement);
            predicate.AddTransition(LinguisticStructureType.SubjectComplement, "final");

            predicate.AddTransition(LinguisticStructureType.AdjectiveComplement, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.AdjectiveComplement, "final");

            predicate.AddTransition(LinguisticStructureType.AdverbialComplement, "final");
        }


        private void AddSubjectComplement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var subjectComplement = builder.AddSubState(objectType);

            AddNounElement(subjectComplement, LinguisticStructureType.NounElement, recursion);
            AddAdjectiveElement(subjectComplement, LinguisticStructureType.AdjectiveElement, recursion);

            subjectComplement.AddTransition("init", LinguisticStructureType.NounElement);
            subjectComplement.AddTransition("init", LinguisticStructureType.AdjectiveElement);
            subjectComplement.AddTransition(LinguisticStructureType.NounElement, "final");
            subjectComplement.AddTransition(LinguisticStructureType.AdjectiveElement, "final");
        }

        private void AddObjectComplement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectComplement = builder.AddSubState(objectType);

            AddNounElement(objectComplement, LinguisticStructureType.NounElement, recursion);
            AddAdjectiveElement(objectComplement, LinguisticStructureType.AdjectiveElement, recursion);

            objectComplement.AddTransition("init", LinguisticStructureType.NounElement);
            objectComplement.AddTransition("init", LinguisticStructureType.AdjectiveElement);
            objectComplement.AddTransition(LinguisticStructureType.NounElement, "final");
            objectComplement.AddTransition(LinguisticStructureType.AdjectiveElement, "final");
        }


        private void AddAdverbialComplement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, LinguisticStructureType.PrepositionalPhrase, recursion);
            AddAdverbElement(adverbialComplement, LinguisticStructureType.AdverbElement, recursion);

            adverbialComplement.AddTransition("init", LinguisticStructureType.PrepositionalPhrase);
            adverbialComplement.AddTransition("init", LinguisticStructureType.AdverbElement);
            adverbialComplement.AddTransition(LinguisticStructureType.PrepositionalPhrase, "final");
            adverbialComplement.AddTransition(LinguisticStructureType.AdverbElement, "final");
        }

        private void AddAdjectiveComplement(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var adverbialComplement = builder.AddSubState(objectType);

            AddPrepositionalPhrase(adverbialComplement, LinguisticStructureType.PrepositionalPhrase, recursion);
            AddPrepositionalPhrase(adverbialComplement, LinguisticStructureType.InfinitivePhrase, recursion);

            adverbialComplement.AddTransition("init", LinguisticStructureType.PrepositionalPhrase);
            adverbialComplement.AddTransition("init", LinguisticStructureType.InfinitivePhrase);

            adverbialComplement.AddTransition(LinguisticStructureType.PrepositionalPhrase, "final");

            adverbialComplement.AddTransition(LinguisticStructureType.InfinitivePhrase, "final");
        }

        private void AddPrepositionalPhrase(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddLexemeStates("E");

            AddNounElement(objectOfPreposition, LinguisticStructureType.ObjectOfPreposition, recursion);

            objectOfPreposition.AddTransition("init", "E", EnglishAttributes.E.Lexeme.Preposition);
            objectOfPreposition.AddTransition("E", LinguisticStructureType.ObjectOfPreposition);
            objectOfPreposition.AddTransition(LinguisticStructureType.ObjectOfPreposition, "final");
        }

        private void AddInfinitivePhrase(GrammarMachineBuilder builder, LinguisticStructureType objectType, int recursion)
        {
            using var _t = Trace.Entering();

            if (--recursion == 0) return;

            var objectOfPreposition = builder.AddSubState(objectType)
                .AddLexemeStates("to");

            AddPredicateElement(objectOfPreposition, LinguisticStructureType.Predicate, recursion);

            objectOfPreposition.AddTransition("init", "to", EnglishAttributes.I.Lexeme.Verb.InfinitiveMarker);
            objectOfPreposition.AddTransition("to", LinguisticStructureType.Predicate);
            objectOfPreposition.AddTransition(LinguisticStructureType.Predicate, "final");
        }


    }
}
