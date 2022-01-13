using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachineBuilder
    {
        private MultiMachine<LinguisticState, IWord> myMachine;
        private LinguisticState myParentState;
        private bool myIsSubState;

        public GrammarMachineBuilder(MultiMachine<LinguisticState, IWord> machine)
        {
            myMachine = machine;

            var initState = new LinguisticState("init", LinguisticStructureType.Undefined);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState("final", LinguisticStructureType.Undefined);
            myMachine.AddFinalState(finalState);
        }

        private GrammarMachineBuilder(MultiMachine<LinguisticState, IWord> machine, LinguisticState parentState)
        {
            myMachine = machine;
            myParentState = parentState;
            myIsSubState = true;

            var initState = new LinguisticState($"{myParentState.Id}.init", LinguisticStructureType.Undefined);
            myMachine.AddInitialSubState(myParentState, initState);

            var finalState = new LinguisticState($"{myParentState.Id}.final", LinguisticStructureType.Undefined);
            myMachine.AddFinalSubState(myParentState, finalState);
        }

        public GrammarMachineBuilder AddLexemeStates(params string[] stateIds)
        {
            foreach (var id in stateIds)
            {
                AddState(id, LinguisticStructureType.Lexeme);
            }

            return this;
        }

        public GrammarMachineBuilder AddState(string stateId, LinguisticStructureType stateType)
        {
            if (myIsSubState)
            {
                var stateToUse = new LinguisticState($"{myParentState.Id}.{stateId}", stateType);
                myMachine.AddSubState(myParentState, stateToUse);
            }
            else
            {
                var stateToUse = new LinguisticState(stateId, stateType);
                myMachine.AddState(stateToUse);
            }

            
            return this;
        }

        public GrammarMachineBuilder AddSubState(LinguisticStructureType subStateType)
        {
            var newSubState = myIsSubState ? new LinguisticState($"{myParentState.Id}.{subStateType}", subStateType) : new LinguisticState(subStateType.ToString(), subStateType);
            myMachine.AddState(newSubState);
            var result = new GrammarMachineBuilder(myMachine, newSubState);

            return result;
        }


        public GrammarMachineBuilder AddTransition(LinguisticStructureType from, LinguisticStructureType to) => AddTransition(from.ToString(), to.ToString());

        public GrammarMachineBuilder AddTransition(string fromId, LinguisticStructureType to) => AddTransition(fromId, to.ToString());

        public GrammarMachineBuilder AddTransition(LinguisticStructureType from, string toId) => AddTransition(from.ToString(), toId);

        public GrammarMachineBuilder AddTransition(string fromId, string toId)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse) ?? throw new InvalidOperationException($"State '{fromIdToUse}' was not found.");
            var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse) ?? throw new InvalidOperationException($"State '{toIdToUse}' was not found.");

            myMachine.AddTransition(fromToUse, toToUse);
            return this;
        }

        public GrammarMachineBuilder AddTransition(string fromId, string toId, params BigInteger[] triggers)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse) ?? throw new InvalidOperationException($"State '{fromIdToUse}' was not found.");
            var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse) ?? throw new InvalidOperationException($"State '{toIdToUse}' was not found.");

            foreach (var trigger in triggers)
            {
                var triggerRule = WordRule.WordContainsAttribute(trigger);
                myMachine.AddTransition(fromToUse, toToUse, triggerRule);
            }

            return this;
        }


        public GrammarMachineBuilder AddTransition(string fromId, string toId, params string[] triggers)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse) ?? throw new InvalidOperationException($"State '{fromIdToUse}' was not found.");
            var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse) ?? throw new InvalidOperationException($"State '{toIdToUse}' was not found.");

            foreach (var trigger in triggers)
            {
                var triggerRule = WordRule.WordStringIs(trigger);
                myMachine.AddTransition(fromToUse, toToUse, triggerRule);
            }

            return this;
        }


    }
}
