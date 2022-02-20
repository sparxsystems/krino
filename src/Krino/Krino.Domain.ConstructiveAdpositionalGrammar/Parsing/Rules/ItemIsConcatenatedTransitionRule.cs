﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    internal class ItemIsConcatenatedTransitionRule : TransitionRule<LinguisticState, IWord>
    {
        public override bool Evaluate(StatePath<LinguisticState, IWord> stateTrace, LinguisticState fromState, LinguisticState toState, IWord trigger)
        {
            var beforeElement = stateTrace.Path.Reverse().FirstOrDefault(x => !x.Definition.Value.Id.StartsWith(fromState.Id));
            var result = beforeElement != null && GrammarAttributes.Morpheme.Free.Functional.Conjunction.Coordinating.IsIn(beforeElement.Definition.Value.Attributes);
            
            return result;
        }

    }
}
