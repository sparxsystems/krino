using Krino.Vertical.Utils.StateMachines;
using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachineBuilder
    {
        private MultiMachine<LinguisticState, BigInteger> myMachine;
        private LinguisticState myState;
        private bool myIsSubState;

        public GrammarMachineBuilder(MultiMachine<LinguisticState, BigInteger> machine)
        {
            myMachine = machine;

            var initState = new LinguisticState("init", LinguisticStructureType.Undefined);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState("final", LinguisticStructureType.Undefined);
            myMachine.AddInitialState(finalState);
        }

        private GrammarMachineBuilder(MultiMachine<LinguisticState, BigInteger> machine, LinguisticState state)
        {
            myMachine = machine;
            myState = state;
            myIsSubState = true;

            var initState = new LinguisticState($"{myState.Id}.init", LinguisticStructureType.Undefined);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState($"{myState.Id}.final", LinguisticStructureType.Undefined);
            myMachine.AddInitialState(finalState);
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
            var stateToUse = myIsSubState ? new LinguisticState($"{myState.Id}.{stateId}", stateType) : new LinguisticState(stateId, stateType);
            myMachine.AddState(stateToUse);
            return this;
        }

        public GrammarMachineBuilder AddSubState(string subStateId, LinguisticStructureType subStateType)
        {
            var newSubState = myIsSubState ? new LinguisticState($"{myState.Id}.{subStateId}", subStateType) : new LinguisticState(subStateId, subStateType);
            var result = new GrammarMachineBuilder(myMachine, newSubState);

            return result;
        }

        public GrammarMachineBuilder AddTransition(string fromId, string toId)
        {
            var fromIdToUse = myIsSubState ? $"{myState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse) ?? throw new InvalidOperationException($"State '{fromIdToUse}' was not found.");
            var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse) ?? throw new InvalidOperationException($"State '{toIdToUse}' was not found.");

            myMachine.AddTransition(fromToUse, toToUse);
            return this;
        }

        public GrammarMachineBuilder AddTransition(string fromId, string toId, params BigInteger[] triggers)
        {
            var fromIdToUse = myIsSubState ? $"{myState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse) ?? throw new InvalidOperationException($"State '{fromIdToUse}' was not found.");
            var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse) ?? throw new InvalidOperationException($"State '{toIdToUse}' was not found.");

            foreach (var trigger in triggers)
            {
                myMachine.AddTransition(fromToUse, toToUse, trigger);
            }

            return this;
        }

        
    }
}
