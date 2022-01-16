using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
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
        private MultiMachine<LinguisticState, IWord> myMachine;

        public EnglishMachine()
        {
            myMachine = new MultiMachine<LinguisticState, IWord>();

            var root = new GrammarMachineBuilder(myMachine);

            var declarativeClause = root.AddSubState(LinguisticStructureType.DeclarativeClause);

            AddNounElement(declarativeClause, LinguisticStructureType.Subject);
            AddPredicateElement(declarativeClause, LinguisticStructureType.Predicate);
            
            declarativeClause.AddTransition("init", LinguisticStructureType.Subject);
            declarativeClause.AddTransition(LinguisticStructureType.Subject, LinguisticStructureType.Predicate);
            declarativeClause.AddTransition(LinguisticStructureType.Predicate, "final");


            root.AddTransition("init", LinguisticStructureType.DeclarativeClause)
                .AddTransition(LinguisticStructureType.DeclarativeClause, "final");

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;

        private void AddPredicateElement(GrammarMachineBuilder builder, LinguisticStructureType objectType)
        {
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

            AddNounElement(predicate, LinguisticStructureType.IndirectObject);
            AddNounElement(predicate, LinguisticStructureType.DirectObject);
            
            //AddNounElement(predicate, LinguisticStructureType.AdverbialComplement, EnglishAttributes.E.Lexeme.Adverb | EnglishAttributes.E.Lexeme.Preposition);

            predicate.AddTransition("init", LinguisticStructureType.Verb);

            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.IndirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent);
            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.DirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent);
            predicate.AddTransition(LinguisticStructureType.Verb, "final");

            predicate.AddTransition(LinguisticStructureType.IndirectObject, LinguisticStructureType.DirectObject);

            //predicate.AddTransition(LinguisticStructureType.DirectObject, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.DirectObject, "final");

            

            //predicate.AddTransition(LinguisticStructureType.AdverbialComplement, "final");
        }


        private void AddNounElement(GrammarMachineBuilder builder, LinguisticStructureType objectType)
        {
            var objectBuilder = builder.AddSubState(objectType)
                .AddLexemeStates("Determiner", "Noun", "Pronoun", "U");

            AddAdjectiveElement(objectBuilder, LinguisticStructureType.AttributiveAdjective);
            AddAdjectiveElement(objectBuilder, LinguisticStructureType.PredicativeAdjective);

            objectBuilder.AddTransition("init", LinguisticStructureType.AttributiveAdjective)
                .AddTransition("init", "Determiner", EnglishAttributes.A.Lexeme.Determiner)
                .AddTransition("init", "Noun", EnglishAttributes.O.Lexeme.Noun)
                .AddTransition("init", "Pronoun", EnglishAttributes.O.Lexeme.Pronoun)

                .AddTransition(LinguisticStructureType.AttributiveAdjective, "Noun", EnglishAttributes.O.Lexeme.Noun)

                .AddTransition("Determiner", "Noun", EnglishAttributes.O.Lexeme.Noun)

                .AddTransition("Pronoun", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("Pronoun", "final")

                .AddTransition("Noun", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("Noun", LinguisticStructureType.PredicativeAdjective)
                .AddTransition("Noun", "final")

                .AddTransition(LinguisticStructureType.PredicativeAdjective, "final");

        }

        private void AddAdjectiveElement(GrammarMachineBuilder builder, LinguisticStructureType objectType)
        {
            var adjectiveBuilder = builder.AddSubState(objectType)
                .AddLexemeStates("A", "U");

            AddAdverbElement(adjectiveBuilder, LinguisticStructureType.Adverb);

            adjectiveBuilder.AddTransition("init", "A", EnglishAttributes.A.Lexeme.Adjective)
                .AddTransition("init", LinguisticStructureType.Adverb)
                .AddTransition(LinguisticStructureType.Adverb, "A")
                .AddTransition("A", "U", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("A", "final")
                .AddTransition("U", "A", EnglishAttributes.A.Lexeme.Adjective)
                .AddTransition("U", LinguisticStructureType.Adverb);
        }

        private void AddAdverbElement(GrammarMachineBuilder builder, LinguisticStructureType objectType)
        {
            builder.AddSubState(objectType)
                .AddLexemeStates("E")
                .AddTransition("init", "E", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("E", "final");
        }
    }
}
