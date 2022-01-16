using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachine
    {
        private class StateTraceComparer : IEqualityComparer<StateTrace<LinguisticState, IWord>>
        {
            public bool Equals(StateTrace<LinguisticState, IWord> x, StateTrace<LinguisticState, IWord> y)
            {
                var xPath = GetRelevantPath(x.Trace).Select(x => x.Definition.Value.Id);
                var yPath = GetRelevantPath(y.Trace).Select(x => x.Definition.Value.Id);
                var result = xPath.SequenceEqual(yPath);
                return result;
            }

            public int GetHashCode(StateTrace<LinguisticState, IWord> obj)
            {
                int hash = 486187739;

                var path = GetRelevantPath(obj.Trace).Select(x => x.Definition.Value.Id);
                foreach (var state in path)
                {
                    hash = (hash * 16777619) ^ state.GetHashCode();
                }

                return hash;
            }

            private IEnumerable<StateItem<LinguisticState, IWord>> GetRelevantPath(IEnumerable<StateItem<LinguisticState, IWord>> o)
                => o.Where(x => x.Definition.StateKind != StateKind.Initial && x.Definition.StateKind != StateKind.Final);
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

            var comparer = new StateTraceComparer();
            var relevantActiveStates = myMachine.GetActiveStates().Distinct(comparer);

            //var kk = relevantActiveStates.Select(x => string.Join(" -> ", x.GetPathToRoot().Reverse().Select(y => y.Value.Definition.Value.Id)));

            foreach (var activeState in relevantActiveStates)
            {
                var text = new Text();

                var stack = new Stack<ILinguisticStructure>();
                
                var sentence = new Sentence(0);
                stack.Push(sentence);

                foreach (var state in activeState.Trace)
                {
                    if (state.Definition.IsSubstate)
                    {
                        if (state.Definition.StateKind == StateKind.Initial)
                        {
                            var structure = state.Definition.Parent.Type.GetLinguisticStructure();

                            var parent = stack.Peek();
                            parent.AddSubStructure(structure);

                            stack.Push(structure);
                        }
                        else if (state.Definition.StateKind == StateKind.Final)
                        {
                            var completedStructure = stack.Pop();
                        }
                        else
                        {
                            var parent = stack.Peek();
                            parent.AddSubStructure(state.ByTrigger);
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
