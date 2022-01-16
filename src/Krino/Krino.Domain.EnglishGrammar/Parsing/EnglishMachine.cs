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
            AddPredicate(declarativeClause);
            declarativeClause.AddTransition("init", LinguisticStructureType.Subject);
            declarativeClause.AddTransition(LinguisticStructureType.Subject, LinguisticStructureType.Predicate);
            declarativeClause.AddTransition(LinguisticStructureType.Predicate, "final");


            root.AddTransition("init", LinguisticStructureType.DeclarativeClause)
                .AddTransition(LinguisticStructureType.DeclarativeClause, "final");

            myMachine.Reset();
        }

        public MultiMachine<LinguisticState, IWord> Machine => myMachine;

        private void AddPredicate(GrammarMachineBuilder builder)
        {
            var predicate = builder.AddSubState(LinguisticStructureType.Predicate);

            predicate.AddSubState(LinguisticStructureType.Verb)
                .AddLexemeStates("I", "E", "U")
                .AddTransition("init", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("I", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("I", "E", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("I", "U", EnglishAttributes.U.Lexeme.Conjunction)
                .AddTransition("I", "final")
                .AddTransition("E", "I", EnglishAttributes.I.Lexeme.Verb)
                .AddTransition("U", "I", EnglishAttributes.I.Lexeme.Verb);

            AddNounElement(predicate, LinguisticStructureType.DirectObject);
            AddNounElement(predicate, LinguisticStructureType.IndirectObject);
            //AddNounElement(predicate, LinguisticStructureType.AdverbialComplement, EnglishAttributes.E.Lexeme.Adverb | EnglishAttributes.E.Lexeme.Preposition);

            predicate.AddTransition("init", LinguisticStructureType.Verb);
            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.DirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent);
            predicate.AddTransitionWithPreviousWordRule(LinguisticStructureType.Verb, LinguisticStructureType.IndirectObject, EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent, EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent);
            predicate.AddTransition(LinguisticStructureType.Verb, "final");

            predicate.AddTransition(LinguisticStructureType.IndirectObject, LinguisticStructureType.DirectObject);

            //predicate.AddTransition(LinguisticStructureType.DirectObject, LinguisticStructureType.AdverbialComplement);
            predicate.AddTransition(LinguisticStructureType.DirectObject, "final");

            

            //predicate.AddTransition(LinguisticStructureType.AdverbialComplement, "final");
        }


        private void AddNounElement(GrammarMachineBuilder builder, LinguisticStructureType objectType, BigInteger initFilter = default)
        {
            var objectBuilder = builder.AddSubState(objectType)
                .AddLexemeStates("O", "A", "E1", "E2", "E3", "U1", "U2", "U3");

            if (initFilter == 0 || EnglishAttributes.O.Lexeme.IsIn(initFilter))
            {
                objectBuilder.AddTransition("init", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun);
            }
            if (initFilter == 0 || EnglishAttributes.A.Lexeme.IsIn(initFilter))
            {
                objectBuilder.AddTransition("init", "A", EnglishAttributes.A.Lexeme);
            }
            if (initFilter == 0 || EnglishAttributes.E.Lexeme.Preposition.IsIn(initFilter))
            {
                objectBuilder.AddTransition("init", "E2", EnglishAttributes.E.Lexeme.Preposition);
            }

            objectBuilder.AddTransition("O", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun)
                .AddTransition("O", "A", EnglishAttributes.A.Lexeme)
                .AddTransition("O", "E2", EnglishAttributes.E.Lexeme.Preposition)
                .AddTransition("O", "E3", EnglishAttributes.E.Lexeme.Postposition)
                .AddTransition("O", "U1", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("O", "final")
                .AddTransition("A", "A", EnglishAttributes.A.Lexeme)
                .AddTransition("A", "U2", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("A", "O", EnglishAttributes.O.Lexeme.Noun)
                .AddTransition("E1", "E1", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("E1", "U3", EnglishAttributes.U.Lexeme.Conjunction.Coordinating)
                .AddTransition("E1", "A", EnglishAttributes.A.Lexeme)
                .AddTransition("E2", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun)
                .AddTransition("E2", "A", EnglishAttributes.A.Lexeme)
                .AddTransition("E2", "E1", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("U1", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun)
                .AddTransition("U2", "A", EnglishAttributes.A.Lexeme)
                .AddTransition("U3", "E1", EnglishAttributes.E.Lexeme.Adverb)
                .AddTransition("E3", "final");
        }
    }
}
