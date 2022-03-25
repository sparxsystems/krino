using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class StatePathExt
    {
        public static IText GetText(this IEnumerable<StateItem<LinguisticState, IWord>> statePath) => (IText)statePath.GetLinguisticStructures().FirstOrDefault();

        /// <summary>
        /// Iterates a path from the grammar machine and creates linguistic structures.
        /// </summary>
        /// <param name="statePath"></param>
        /// <returns></returns>
        public static IEnumerable<ILinguisticStructure> GetLinguisticStructures(this IEnumerable<StateItem<LinguisticState, IWord>> statePath)
        {
            var result = new List<ILinguisticStructure>();

            // Factory creating the linguistic structure.
            var linguisticStructureFactory = new LinguisticStructureFactory();

            var stack = new Stack<ILinguisticStructure>();

            foreach (var state in statePath)
            {
                if (state.Definition.IsSubstate)
                {
                    if (state.Definition.StateKind == StateKind.Initial)
                    {
                        // Note: attributes of parent state. And the parent state represents the linguistic structure in which we are.
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
            // Get portion of the path which starts with the verb phrase.
            var verbPhrasePath = statePath.TakeFromLastOccuranceOf(x => x.Definition.StateKind == StateKind.Initial && GrammarAttributes.Phrase.VerbPhrase.IsIn(x.Definition.Parent.Attributes));
            
            var verbPhrase = verbPhrasePath.GetLinguisticStructures().FirstOrDefault() as IPhrase;

            return verbPhrase;
        }
    }
}
