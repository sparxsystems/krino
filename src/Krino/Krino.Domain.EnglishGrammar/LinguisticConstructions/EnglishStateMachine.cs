using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing;
using Krino.Domain.EnglishGrammar.Morphemes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Domain.EnglishGrammar.LinguisticConstructions
{
    public class EnglishStateMachine
    {
        private MultiMachine<string, BigInteger> myStateMachine;

        public EnglishStateMachine()
        {
            myStateMachine = new MultiMachine<string, BigInteger>();

            myStateMachine.AddInitialState("start");
            
            

            AddQuestioning();

            
            myStateMachine.AddFinalState("end");

            myStateMachine.AddTransition("start", "clause");
            myStateMachine.AddTransition("clause", "end");
        }

        private void AddClause()
        {
            myStateMachine.AddState("clause");
            myStateMachine.AddInitialSubState("clause", "clause.init");



            myStateMachine.AddFinalSubState("clause", "clause.final");
        }

        private void AddQuestioning()
        {
            myStateMachine.AddSubState("clause", "clause.questioning");

            myStateMachine.AddInitialSubState("clause.questioning", "clause.questioning.init");
            myStateMachine.AddSubState("clause.questioning", "clause.questioning.O");
            myStateMachine.AddSubState("clause.questioning", "clause.questioning.I");
            myStateMachine.AddFinalSubState("clause.questioning", "clause.questioning.final");

            myStateMachine.AddTransition("clause.questioning.init", "clause.questioning.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun.Interrogative));
            myStateMachine.AddTransition("clause.questioning.init", "clause.questioning.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition("clause.questioning.O", "clause.questioning.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition("clause.questioning.I", "clause.questioning.final", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Pronoun | EnglishAttributes.A.Lexeme | EnglishAttributes.E.Lexeme.Adverb));

            myStateMachine.AddTransition("clause", "clause.questioning");
            myStateMachine.AddTransition("clause", "clause.questioning");
        }

        private void AddObject(string state)
        {

        }
    }
}
