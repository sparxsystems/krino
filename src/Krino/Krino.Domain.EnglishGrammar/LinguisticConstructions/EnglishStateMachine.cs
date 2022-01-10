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
            myStateMachine.AddInitialState("init");
            myStateMachine.AddFinalState("final");

            AddClause();
            myStateMachine.AddTransition("init", "clause");
            myStateMachine.AddTransition("clause", "final");

            myStateMachine.Reset();
        }

        public MultiMachine<string, BigInteger> Machine => myStateMachine;

        private void AddClause(string parentState, string subState)
        {
            var subStateName = AddSubState(parentState, subState);

            // Statement starting with a subject.
            AddObject(subStateName, "subject1");
            AddPredicate(subStateName, "predicate2");

            // Question starting with a verb.
            AddPredicate(subStateName, "predicate1");
            AddObject(subStateName, "subject2");

            // Question starting with a pronoun.
            AddQuestioning(subStateName, "questioning");
            

            // Valency 2
            AddObject(subStateName, "direct_object");

            // Valency 3
            AddObject(subStateName, "indirect_object");

            // Adverbial
            AddObject(subStateName, "adverbial", EnglishAttributes.E.Lexeme.Preposition);



            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.subject1");
            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.predicate1");
            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.questioning");

            myStateMachine.AddTransition($"{subStateName}.questioning", $"{subStateName}.subject1");

            myStateMachine.AddTransition($"{subStateName}.subject1", $"{subStateName}.predicate2");

            myStateMachine.AddTransition($"{subStateName}.predicate2", $"{subStateName}.direct_object");
            myStateMachine.AddTransition($"{subStateName}.predicate2", $"{subStateName}.adverbial");
            myStateMachine.AddTransition($"{subStateName}.predicate2", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.predicate1", $"{subStateName}.subject2");

            myStateMachine.AddTransition($"{subStateName}.subject2", $"{subStateName}.direct_object");
            myStateMachine.AddTransition($"{subStateName}.subject2", $"{subStateName}.adverbial");
            myStateMachine.AddTransition($"{subStateName}.subject2", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.direct_object", $"{subStateName}.indirect_object");
            myStateMachine.AddTransition($"{subStateName}.direct_object", $"{subStateName}.adverbial");
            myStateMachine.AddTransition($"{subStateName}.direct_object", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.indirect_object", $"{subStateName}.adverbial");
            myStateMachine.AddTransition($"{subStateName}.indirect_object", $"{subStateName}.final");

        }

        private void AddQuestioning(string parentState, string subState)
        {
            var subStateName = AddSubState(parentState, subState);

            myStateMachine.AddSubState(subStateName, $"{subStateName}.O");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.I");

            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun.Interrogative));
            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.final");
        }

        private void AddObject(string parentState, string subState, BigInteger initTriggerFilter = default)
        {
            var subStateName = AddSubState(parentState, subState);

            myStateMachine.AddSubState(subStateName, $"{subStateName}.O");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.A");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.E1");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.E2");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.E3");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.U1");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.U2");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.U3");
            //myStateMachine.AddSubState(objectName, $"{objectName}.U4");

            initTriggerFilter = initTriggerFilter != default ? initTriggerFilter :
                EnglishAttributes.O.Lexeme.Noun | EnglishAttributes.O.Lexeme.Pronoun | EnglishAttributes.A.Lexeme | EnglishAttributes.E.Lexeme.Adverb | EnglishAttributes.E.Lexeme.Preposition;

            if (EnglishAttributes.O.Lexeme.Noun.IsIn(initTriggerFilter))
            {
                myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun));
            }
            if (EnglishAttributes.O.Lexeme.Pronoun.IsIn(initTriggerFilter))
            {
                myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun));
            }
            if (EnglishAttributes.A.Lexeme.IsIn(initTriggerFilter))
            {
                myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.A", ValueIsInRule.Is(EnglishAttributes.A.Lexeme));
            }
            if (EnglishAttributes.E.Lexeme.Adverb.IsIn(initTriggerFilter))
            {
                myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.E1", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));
            }
            if (EnglishAttributes.E.Lexeme.Preposition.IsIn(initTriggerFilter))
            {
                myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.E2", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Preposition));
            }
            
            

            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.E2", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Preposition));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.U1", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction.Coordinating));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.E3", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Postposition));
            //myStateMachine.AddTransition($"{objectName}.O", $"{objectName}.U4", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction.Subordinating));
            myStateMachine.AddTransition($"{subStateName}.O", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.A", $"{subStateName}.A", ValueIsInRule.Is(EnglishAttributes.A.Lexeme));
            myStateMachine.AddTransition($"{subStateName}.A", $"{subStateName}.U2", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction.Coordinating));
            myStateMachine.AddTransition($"{subStateName}.A", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun));

            myStateMachine.AddTransition($"{subStateName}.E1", $"{subStateName}.E1", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));
            myStateMachine.AddTransition($"{subStateName}.E1", $"{subStateName}.U3", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction.Coordinating));
            myStateMachine.AddTransition($"{subStateName}.E1", $"{subStateName}.A", ValueIsInRule.Is(EnglishAttributes.A.Lexeme));

            myStateMachine.AddTransition($"{subStateName}.E2", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun));
            myStateMachine.AddTransition($"{subStateName}.E2", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun));
            myStateMachine.AddTransition($"{subStateName}.E2", $"{subStateName}.A", ValueIsInRule.Is(EnglishAttributes.A.Lexeme));
            myStateMachine.AddTransition($"{subStateName}.E2", $"{subStateName}.E1", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));

            myStateMachine.AddTransition($"{subStateName}.E3", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.U1", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Noun));
            myStateMachine.AddTransition($"{subStateName}.U1", $"{subStateName}.O", ValueIsInRule.Is(EnglishAttributes.O.Lexeme.Pronoun));
            myStateMachine.AddTransition($"{subStateName}.U2", $"{subStateName}.A", ValueIsInRule.Is(EnglishAttributes.A.Lexeme));
            myStateMachine.AddTransition($"{subStateName}.U3", $"{subStateName}.E1", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));

            // Recursion
            //myStateMachine.AddTransition($"{objectName}.U4", "clause");
        }

        private void AddPredicate(string parentState, string subState)
        {
            var subStateName = AddSubState(parentState, subState);

            myStateMachine.AddSubState(subStateName, $"{subStateName}.I");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.E");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.U");


            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));

            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.E", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.U", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.E", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));

            myStateMachine.AddTransition($"{subStateName}.U", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
        }

        private void AddAdverbial(string parentState, string subState)
        {
            var subStateName = AddSubState(parentState, subState);

            myStateMachine.AddSubState(subStateName, $"{subStateName}.I");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.E");
            myStateMachine.AddSubState(subStateName, $"{subStateName}.U");


            myStateMachine.AddTransition($"{subStateName}.init", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));

            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.E", ValueIsInRule.Is(EnglishAttributes.E.Lexeme.Adverb));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.U", ValueIsInRule.Is(EnglishAttributes.U.Lexeme.Conjunction));
            myStateMachine.AddTransition($"{subStateName}.I", $"{subStateName}.final");

            myStateMachine.AddTransition($"{subStateName}.E", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));

            myStateMachine.AddTransition($"{subStateName}.U", $"{subStateName}.I", ValueIsInRule.Is(EnglishAttributes.I.Lexeme.Verb));
        }

        private string AddSubState(string parentState, string subState)
        {
            var fullSubStateName = string.Join(".", parentState, subState);

            myStateMachine.AddSubState(parentState, fullSubStateName);
            myStateMachine.AddInitialSubState(fullSubStateName, $"{fullSubStateName}.init");
            myStateMachine.AddFinalSubState(fullSubStateName, $"{fullSubStateName}.final");

            return fullSubStateName;
        }
    }
}
