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
        private MultiMachine<LinguisticState, BigInteger> myMachine;

        public EnglishMachine()
        {
            myMachine = new MultiMachine<LinguisticState, BigInteger>();

            var builder = new GrammarMachineBuilder(myMachine);
            builder.AddSubState("declarative_clause", LinguisticStructureType.Clause)
                .AddTransition("init", "declarative_clause")
                .AddTransition("declarative_clause", "final")
                .AddSubState("subject", LinguisticStructureType.Subject)
                    .AddLexemeStates("O", "A", "E1", "E2", "E3", "U1", "U2", "U3")
                    .AddTransition("init", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun)
                    .AddTransition("init", "A", EnglishAttributes.A.Lexeme)
                    .AddTransition("init", "E2", EnglishAttributes.E.Lexeme.Preposition)
                    .AddTransition("O", "O", EnglishAttributes.O.Lexeme.Noun, EnglishAttributes.O.Lexeme.Pronoun)
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


            myMachine.Reset();
        }


       
    }
}
