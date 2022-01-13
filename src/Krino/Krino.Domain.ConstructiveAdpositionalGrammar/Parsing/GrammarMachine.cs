using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachine
    {
        private class StatePathComparer : IEqualityComparer<ITree<StateRecord<LinguisticState, IWord>>>
        {
            public bool Equals(ITree<StateRecord<LinguisticState, IWord>> x, ITree<StateRecord<LinguisticState, IWord>> y)
            {
                var xPath = GetRelevantPath(x);
                var yPath = GetRelevantPath(y);
                var result = xPath.Select(x => x.Value.Definition.Value.Id).SequenceEqual(yPath.Select(x => x.Value.Definition.Value.Id));
                return result;
            }

            public int GetHashCode(ITree<StateRecord<LinguisticState, IWord>> obj)
            {
                int hash = 486187739;

                var path = GetRelevantPath(obj);
                foreach (var state in path)
                {
                    hash = (hash * 16777619) ^ state.Value.Definition.Value.Id.GetHashCode();
                }

                return hash;
            }

            private IEnumerable<ITree<StateRecord<LinguisticState, IWord>>> GetRelevantPath(ITree<StateRecord<LinguisticState, IWord>> o)
                => o.GetPathToRoot().Where(x => x.Value.Definition.StateKind != StateKind.Initial && x.Value.Definition.StateKind != StateKind.Final);
        }

        private MultiMachine<LinguisticState, IWord> myMachine;

        public GrammarMachine(MultiMachine<LinguisticState, IWord> grammarMachine)
        {
            myMachine = grammarMachine;
        }


        public void Add(IWord word)
        {
            myMachine.Fire(word);
        }

        public void Reset() => myMachine.Reset();

        public IEnumerable<IText> GetTexts()
        {
            var result = new List<IText>();

            var comparer = new StatePathComparer();
            var relevantActiveStates = myMachine.ActiveStateRecords.Distinct(comparer);

            //var kk = relevantActiveStates.Select(x => string.Join(" -> ", x.GetPathToRoot().Reverse().Select(y => y.Value.Definition.Value.Id)));

            foreach (var activeState in relevantActiveStates)
            {
                var text = new Text();

                var stack = new Stack<ILinguisticStructure>();
                
                var sentence = new Sentence(0);
                stack.Push(sentence);

                var sequence = activeState.GetPathToRoot().Reverse();
                foreach (var state in sequence)
                {
                    if (state.Value.Definition.IsSubstate)
                    {
                        if (state.Value.Definition.StateKind == StateKind.Initial)
                        {
                            var structure = state.Value.Definition.Parent.Type.GetLinguisticStructure();

                            var parent = stack.Peek();
                            parent.AddSubStructure(structure);

                            stack.Push(structure);
                        }
                        else if (state.Value.Definition.StateKind == StateKind.Final)
                        {
                            var completedStructure = stack.Pop();
                        }
                        else
                        {
                            var parent = stack.Peek();
                            parent.AddSubStructure(state.Value.ByTrigger);
                        }
                    }
                }

                text.Sentences.Add(sentence);

                result.Add(text);
            }


            return result;
        }
    }
}
