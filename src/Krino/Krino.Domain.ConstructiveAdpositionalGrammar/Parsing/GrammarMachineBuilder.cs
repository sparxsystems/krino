using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
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

            var initState = new LinguisticState("init", 0);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState("final", 0);
            myMachine.AddFinalState(finalState);
        }

        private GrammarMachineBuilder(MultiMachine<LinguisticState, IWord> machine, LinguisticState parentState)
        {
            using var _t = Trace.Entering();

            myMachine = machine;
            myParentState = parentState;
            myIsSubState = true;

            var initState = new LinguisticState($"{myParentState.Id}|init", 0);
            myMachine.AddInitialSubState(myParentState, initState);

            var finalState = new LinguisticState($"{myParentState.Id}|final", 0);
            myMachine.AddFinalSubState(myParentState, finalState);
        }

        public LinguisticState ParentState => myParentState;

        public GrammarMachineBuilder AddStates(params BigInteger[] attributes)
        {
            using var _t = Trace.Entering();

            foreach (var state in attributes)
            {
                AddState(state.GetGrammarId(), state);
            }

            return this;
        }

        public GrammarMachineBuilder AddState(string stateId, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            if (myIsSubState)
            {
                var stateToUse = new LinguisticState($"{myParentState.Id}|{stateId}", attributes);
                myMachine.AddSubState(myParentState, stateToUse);
            }
            else
            {
                var stateToUse = new LinguisticState(stateId, attributes);
                myMachine.AddState(stateToUse);
            }

            
            return this;
        }

        public GrammarMachineBuilder AddSubState(BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var newSubState = myIsSubState ? new LinguisticState($"{myParentState.Id}|{attributes.GetGrammarId()}", attributes) : new LinguisticState(attributes.GetGrammarId(), attributes);
            myMachine.AddState(newSubState);
            var result = new GrammarMachineBuilder(myMachine, newSubState);

            return result;
        }


        public GrammarMachineBuilder AddEmptyTransition(BigInteger from, BigInteger to) => AddEmptyTransition(from.GetGrammarId(), to.GetGrammarId());

        public GrammarMachineBuilder AddEmptyTransition(string fromId, BigInteger to) => AddEmptyTransition(fromId, to.GetGrammarId());

        public GrammarMachineBuilder AddEmptyTransition(BigInteger from, string toId) => AddEmptyTransition(from.GetGrammarId(), toId);

        public GrammarMachineBuilder AddEmptyTransition(string fromId, string toId)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                myMachine.AddTransition(from.Value, to.Value);
            }
            
            return this;
        }


        public GrammarMachineBuilder AddTransitionWithPreviousWordIsRule(BigInteger from, BigInteger to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordIsRule(from.GetGrammarId(), to.GetGrammarId(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordIsRule(string fromId, string toId, params BigInteger[] previousWordAttributes)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var attribute in previousWordAttributes)
                {
                    var pathRule = ParsingRule.PreviousWordAttributes(attribute);
                    var triggerRule = ParsingRule.ImmediateTrigger();
                    var transitionRule = new TransitionRule<LinguisticState, IWord>() { PathRule = pathRule, TriggerRule = triggerRule };

                    myMachine.AddTransition(from.Value, to.Value, transitionRule);
                }
            }

            return this;
        }


        public GrammarMachineBuilder AddTransitionWithPreviousWordNotRule(BigInteger from, BigInteger to, params BigInteger[] previousWordAttributes) => AddTransitionWithPreviousWordNotRule(from.GetGrammarId(), to.GetGrammarId(), previousWordAttributes);

        public GrammarMachineBuilder AddTransitionWithPreviousWordNotRule(string fromId, string toId, params BigInteger[] previousWordAttributes)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var attribute in previousWordAttributes)
                {
                    var pathRule = RuleMaker.Not(ParsingRule.PreviousWordAttributes(attribute));
                    var triggerRule = ParsingRule.ImmediateTrigger();
                    var transitionRule = new TransitionRule<LinguisticState, IWord>() { PathRule = pathRule, TriggerRule = triggerRule };

                    myMachine.AddTransition(from.Value, to.Value, transitionRule);
                }
            }

            return this;
        }

        public GrammarMachineBuilder AddTransitionWithVerbPhraseRules(BigInteger from, BigInteger to, params BigInteger[] acceptedAttributes)
            => AddTransitionWithRules(from.GetGrammarId(), to.GetGrammarId(), acceptedAttributes.Select(x => ParsingRule.VerbPhraseAttributesRule(x)).ToArray());

        public GrammarMachineBuilder AddTransitionWithVerbPhraseRules(string fromId, string toId, params BigInteger[] acceptedAttributes)
            => AddTransitionWithRules(fromId, toId, acceptedAttributes.Select(x => ParsingRule.VerbPhraseAttributesRule(x)).ToArray());

        public GrammarMachineBuilder AddTransitionWithRules(string fromId, string toId, params IRule<StatePath<LinguisticState, IWord>>[] pathRules)
        {
            using var _t = Trace.Entering();

            if (pathRules != null && pathRules.Length > 0 && TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var pathRule in pathRules)
                {
                    var triggerRule = ParsingRule.ImmediateTrigger();
                    var transitionRule = new TransitionRule<LinguisticState, IWord>() { PathRule = pathRule, TriggerRule = triggerRule };

                    myMachine.AddTransition(from.Value, to.Value, transitionRule);
                }
            }

            return this;
        }



        public GrammarMachineBuilder AddTriggeredTransition(BigInteger from, BigInteger to) => AddTriggeredTransition(from.GetGrammarId(), to);

        public GrammarMachineBuilder AddTriggeredTransition(string fromId, BigInteger to) => AddTriggeredTransition(fromId, to, ParsingRule.WordContainsAttribute(to));

        public GrammarMachineBuilder AddTriggeredTransition(string fromId, BigInteger to, IRule<IWord> triggerRule)
        {
            using var _t = Trace.Entering();

            var toId = to.GetGrammarId();

            if (TryGetStateDefinitions(fromId, toId, out var fromState, out var toState))
            {
                myMachine.AddTransition(fromState.Value, toState.Value, triggerRule);
            }

            return this;
        }


        private bool TryGetStateDefinitions(string fromId, string toId, out StateDefinition<LinguisticState> from, out StateDefinition<LinguisticState> to)
        {
            using var _t = Trace.Entering();

            from = default;
            to = default;

            var fromIdToUse = myIsSubState ? $"{myParentState.Id}|{fromId}" : fromId;
            var toIdToUse = myIsSubState ? $"{myParentState.Id}|{toId}" : toId;

            if (myMachine.TryGetStateDefinition(new LinguisticState(fromIdToUse, 0), out from))
            {
                if (myMachine.TryGetStateDefinition(new LinguisticState(toIdToUse, 0), out to))
                {
                    return true;
                }
            }

            return false;
        }
    }
}
