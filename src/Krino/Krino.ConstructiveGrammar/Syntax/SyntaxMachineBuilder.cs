using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Syntax
{
    public class SyntaxMachineBuilder
    {
        private MultiMachine<LinguisticState, IWord> myMachine;
        private LinguisticState myParentState;
        private bool myIsSubState;

        public SyntaxMachineBuilder(MultiMachine<LinguisticState, IWord> machine)
        {
            using var _t = Trace.Entering();

            myMachine = machine;

            var initState = new LinguisticState("init", 0);
            myMachine.AddInitialState(initState);

            var finalState = new LinguisticState("final", 0);
            myMachine.AddFinalState(finalState);
        }

        private SyntaxMachineBuilder(MultiMachine<LinguisticState, IWord> machine, LinguisticState parentState)
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

        public SyntaxMachineBuilder AddStates(params BigInteger[] attributes)
        {
            using var _t = Trace.Entering();

            foreach (var state in attributes)
            {
                AddState(state.GetGrammarId(), state);
            }

            return this;
        }

        public SyntaxMachineBuilder AddState(string stateId, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            if (myIsSubState)
            {
                var id = string.Concat(myParentState.Id, "|", stateId);
                var stateToUse = new LinguisticState(id, attributes);
                myMachine.AddSubState(myParentState, stateToUse);
            }
            else
            {
                var stateToUse = new LinguisticState(stateId, attributes);
                myMachine.AddState(stateToUse);
            }

            
            return this;
        }

        public SyntaxMachineBuilder AddSubState(BigInteger attributes) => AddSubState(attributes.GetGrammarId(), attributes);

        public SyntaxMachineBuilder AddSubState(string subStateId, BigInteger attributes)
        {
            using var _t = Trace.Entering();

            var newSubState = myIsSubState ? new LinguisticState(string.Concat(myParentState.Id, "|", subStateId), attributes) : new LinguisticState(subStateId, attributes);
            myMachine.AddState(newSubState);
            var result = new SyntaxMachineBuilder(myMachine, newSubState);

            return result;
        }


        public SyntaxMachineBuilder AddEmptyTransition(BigInteger from, BigInteger to) => AddEmptyTransition(from.GetGrammarId(), to.GetGrammarId());

        public SyntaxMachineBuilder AddEmptyTransition(string fromId, BigInteger to) => AddEmptyTransition(fromId, to.GetGrammarId());

        public SyntaxMachineBuilder AddEmptyTransition(BigInteger from, string toId) => AddEmptyTransition(from.GetGrammarId(), toId);

        public SyntaxMachineBuilder AddEmptyTransition(string fromId, string toId)
        {
            using var _t = Trace.Entering();

            if (TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                myMachine.AddTransition(from.Value, to.Value);
            }
            
            return this;
        }


        public SyntaxMachineBuilder AddEmptyTransitionWithVerbRule(BigInteger from, BigInteger to, params BigInteger[] acceptedAttributes)
            => AddEmptyTransitionWithRules(from.GetGrammarId(), to.GetGrammarId(), acceptedAttributes.Select(x => SyntaxRule.VerbPhraseContainsAttribute(x)).ToArray());

        public SyntaxMachineBuilder AddEmptyTransitionWithRules(string fromId, string toId, params TransitionRule<LinguisticState, IWord>[] transitionRules)
        {
            using var _t = Trace.Entering();

            if (transitionRules != null && transitionRules.Length > 0 && TryGetStateDefinitions(fromId, toId, out var from, out var to))
            {
                foreach (var transitionRule in transitionRules)
                {
                    myMachine.AddTransition(from.Value, to.Value, transitionRule);
                }
            }

            return this;
        }



        public SyntaxMachineBuilder AddTriggeredTransition(BigInteger from, BigInteger to) => AddTriggeredTransition(from.GetGrammarId(), to);

        public SyntaxMachineBuilder AddTriggeredTransition(string fromId, BigInteger to) => AddTriggeredTransition(fromId, to, SyntaxRule.WordContainsAttribute(to));

        public SyntaxMachineBuilder AddTriggeredTransition(string fromId, BigInteger to, IRule<IWord> triggerRule) => AddTriggeredTransition(fromId, to.GetGrammarId(), triggerRule);

        public SyntaxMachineBuilder AddTriggeredTransition(BigInteger from, string toId, IRule<IWord> triggerRule) => AddTriggeredTransition(from.GetGrammarId(), toId, triggerRule);

        public SyntaxMachineBuilder AddTriggeredTransition(string fromId, string toId, IRule<IWord> triggerRule)
        {
            using var _t = Trace.Entering();

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

            var fromIdToUse = myIsSubState ? string.Concat(myParentState.Id, "|", fromId) : fromId;
            var toIdToUse = myIsSubState ? string.Concat(myParentState.Id, "|", toId) : toId;

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
