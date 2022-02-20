using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class StatePathExt
    {
        public static IText GetText(this IEnumerable<StateItem<LinguisticState, IWord>> statePath) => (IText)statePath.GetLinguisticStructures().FirstOrDefault();

        public static IEnumerable<ILinguisticStructure> GetLinguisticStructures(this IEnumerable<StateItem<LinguisticState, IWord>> statePath)
        {
            var result = new List<ILinguisticStructure>();

            var linguisticStructureFactory = new LinguisticStructureFactory();

            var stack = new Stack<ILinguisticStructure>();

            foreach (var state in statePath)
            {
                if (state.Definition.IsSubstate)
                {
                    if (state.Definition.StateKind == StateKind.Initial)
                    {
                        var structure = linguisticStructureFactory.Create(state.Definition.Parent.Attributes);

                        // If the state is not on the root then add the substate into its parent.
                        if (stack.Count > 0)
                        {
                            var parent = stack.Peek();
                            parent.AddSubStructure(structure);
                        }
                        else
                        {
                            // Add the root structure into the result list.
                            result.Add(structure);
                        }

                        stack.Push(structure);
                    }
                    else if (state.Definition.StateKind == StateKind.Final)
                    {
                        stack.Pop();
                    }
                    else
                    {
                        // If it is a triggered state (i.e. a word triggered the state).
                        if (!state.IsImmediateTransition)
                        {
                            var parent = stack.Peek();
                            parent.AddSubStructure(state.ByTrigger);
                        }
                    }
                }
            }

            return result;
        }


        public static IPhrase GetLastVerbPhrase(this IEnumerable<StateItem<LinguisticState, IWord>> statePath)
        {
            var verbPhrasePath = statePath.TakeFromLast(x => x.Definition.StateKind == StateKind.Initial && GrammarAttributes.Phrase.VerbPhrase.IsIn(x.Definition.Parent.Attributes));
            var clause = verbPhrasePath.GetLinguisticStructures().FirstOrDefault() as IPhrase;

            return clause;
        }
    }
}
