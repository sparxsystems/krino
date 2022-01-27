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

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse);
            if (fromToUse != null)
            {
                var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse);
                if (toToUse != null)
                {
                    myMachine.AddTransition(fromToUse, toToUse);
                }
            }
            
            return this;
        }


        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(LinguisticStructureType from, LinguisticStructureType to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(from.ToString(), to.ToString(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(string fromId, LinguisticStructureType to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(fromId, to.ToString(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(LinguisticStructureType from, string toId, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(from.ToString(), toId, previousWordAttributes);
        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(string fromId, string toId, params BigInteger[] previousWordAttributes)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse);
            if (fromToUse != null)
            {
                var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse);
                if (toToUse != null)
                {
                    foreach (var attribute in previousWordAttributes)
                    {
                        var traceRule = ParsingRule.PreviousWordContainsAttribute(attribute);
                        var triggerRule = ParsingRule.GetImmediateTrigger();
                        var transitionRule = new TransitionRule<LinguisticState, IWord>(traceRule, null, null, triggerRule);

                        myMachine.AddTransition(fromToUse, toToUse, transitionRule);
                    }
                }
            }
            
            
            return this;
        }


        public GrammarMachineBuilder AddTransition(LinguisticStructureType from, LinguisticStructureType to, params BigInteger[] triggers) => AddTransition(from.ToString(), to.ToString(), triggers);

        public GrammarMachineBuilder AddTransition(string fromId, LinguisticStructureType to, params BigInteger[] triggers) => AddTransition(fromId, to.ToString(), triggers);

        public GrammarMachineBuilder AddTransition(LinguisticStructureType from, string toId, params BigInteger[] triggers) => AddTransition(from.ToString(), toId, triggers);

        public GrammarMachineBuilder AddTransition(string fromId, string toId, params BigInteger[] triggers)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse);
            if (fromToUse != null)
            {
                var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse);
                if (toToUse != null)
                {
                    foreach (var trigger in triggers)
                    {
                        var triggerRule = ParsingRule.WordContainsAttribute(trigger);
                        myMachine.AddTransition(fromToUse, toToUse, triggerRule);
                    }
                }
            }

            return this;
        }



        public GrammarMachineBuilder AddTransition(string fromId, string toId, params string[] triggers)
        {
            var fromIdToUse = myIsSubState ? $"{myParentState.Id}.{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}.{toId}" : toId;

            var fromToUse = myMachine.States.FirstOrDefault(x => x.Id == fromIdToUse);
            if (fromToUse != null)
            {
                var toToUse = myMachine.States.FirstOrDefault(x => x.Id == toIdToUse);
                if (toToUse != null)
                {
                    foreach (var trigger in triggers)
                    {
                        var triggerRule = ParsingRule.WordStringIs(trigger);
                        myMachine.AddTransition(fromToUse, toToUse, triggerRule);
                    }
                }
            }

            return this;
        }


    }
}
