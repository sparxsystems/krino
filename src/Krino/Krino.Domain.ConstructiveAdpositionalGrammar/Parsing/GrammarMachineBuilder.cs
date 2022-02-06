using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
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
            using var _t = Trace.Entering();

            myMachine = machine;

            var initState = new LinguisticState("init", null);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState("final", null);
            myMachine.AddFinalState(finalState);
        }

        private GrammarMachineBuilder(MultiMachine<LinguisticState, IWord> machine, LinguisticState parentState)
        {
            using var _t = Trace.Entering();

            myMachine = machine;
            myParentState = parentState;
            myIsSubState = true;

            var initState = new LinguisticState($"{myParentState.Id}-init", null);
            myMachine.AddInitialSubState(myParentState, initState);

            var finalState = new LinguisticState($"{myParentState.Id}-final", null);
            myMachine.AddFinalSubState(myParentState, finalState);
        }

        public GrammarMachineBuilder AddStates(params EnumBase[] states)
        {
            using var _t = Trace.Entering();

            foreach (var state in states)
            {
                AddState(state.GetGrammarId(), state);
            }

            return this;
        }

        public GrammarMachineBuilder AddState(string stateId, EnumBase stateType)
        {
            using var _t = Trace.Entering();

            if (myIsSubState)
            {
                var stateToUse = new LinguisticState($"{myParentState.Id}-{stateId}", stateType);
                myMachine.AddSubState(myParentState, stateToUse);
            }
            else
            {
                var stateToUse = new LinguisticState(stateId, stateType);
                myMachine.AddState(stateToUse);
            }

            
            return this;
        }

        public GrammarMachineBuilder AddSubState(EnumBase subStateType)
        {
            using var _t = Trace.Entering();

            var newSubState = myIsSubState ? new LinguisticState($"{myParentState.Id}-{subStateType.GetGrammarId()}", subStateType) : new LinguisticState(subStateType.GetGrammarId(), subStateType);
            myMachine.AddState(newSubState);
            var result = new GrammarMachineBuilder(myMachine, newSubState);

            return result;
        }


        public GrammarMachineBuilder AddTransition(EnumBase from, EnumBase to) => AddTransition(from.GetGrammarId(), to.GetGrammarId());

        public GrammarMachineBuilder AddTransition(string fromId, EnumBase to) => AddTransition(fromId, to.GetGrammarId());

        public GrammarMachineBuilder AddTransition(EnumBase from, string toId) => AddTransition(from.GetGrammarId(), toId);

        public GrammarMachineBuilder AddTransition(string fromId, string toId)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                myMachine.AddTransition(from.Value, to.Value);
            }
            
            return this;
        }


        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(EnumBase from, EnumBase to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(from.GetGrammarId(), to.GetGrammarId(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(string fromId, EnumBase to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(fromId, to.GetGrammarId(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(EnumBase from, string toId, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordRule(from.GetGrammarId(), toId, previousWordAttributes);
        public GrammarMachineBuilder AddTransitionWithPreviousWordRule(string fromId, string toId, params BigInteger[] previousWordAttributes)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var attribute in previousWordAttributes)
                {
                    var traceRule = ParsingRule.PreviousWordContainsAttribute(attribute);
                    var triggerRule = ParsingRule.GetImmediateTrigger();
                    var transitionRule = new TransitionRule<LinguisticState, IWord>(traceRule, null, null, triggerRule);

                    myMachine.AddTransition(from.Value, to.Value, transitionRule);
                }
            }

            return this;
        }


        public GrammarMachineBuilder AddTransition(EnumBase from, EnumBase to, params BigInteger[] triggers) => AddTransition(from.GetGrammarId(), to.GetGrammarId(), triggers);

        public GrammarMachineBuilder AddTransition(string fromId, EnumBase to, params BigInteger[] triggers) => AddTransition(fromId, to.GetGrammarId(), triggers);

        public GrammarMachineBuilder AddTransition(EnumBase from, string toId, params BigInteger[] triggers) => AddTransition(from.GetGrammarId(), toId, triggers);

        public GrammarMachineBuilder AddTransition(string fromId, string toId, params BigInteger[] triggers)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var trigger in triggers)
                {
                    var triggerRule = ParsingRule.WordContainsAttribute(trigger);
                    myMachine.AddTransition(from.Value, to.Value, triggerRule);
                }
            }

            return this;
        }


        private bool TryGetStateDefinitions(string fromId, string toId, out StateDefinition<LinguisticState> from, out StateDefinition<LinguisticState> to)
        {
            using var _t = Trace.Entering();

            from = default;
            to = default;

            var fromIdToUse = myIsSubState ? $"{myParentState.Id}-{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}-{toId}" : toId;

            if (myMachine.TryGetStateDefinition(new LinguisticState(fromIdToUse, null), out from))
            {
                if (myMachine.TryGetStateDefinition(new LinguisticState(toIdToUse, null), out to))
                {
                    return true;
                }
            }

            return false;
        }
    }
}
