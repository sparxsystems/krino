using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.StateMachines;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachine
    {
        private MultiMachine<LinguisticState, IWord> myMachine;

        public GrammarMachine(MultiMachine<LinguisticState, IWord> grammarMachine)
        {
            myMachine = grammarMachine;
        }

        public string DebugView
        {
            get
            {
                var builder = new StringBuilder();

                var activeStates = myMachine.GetActiveStates().ToList();
                for (var i = 0; i < activeStates.Count; ++i)
                {
                    builder.Append(i).Append(" ");

                    var activeState = activeStates[i];

                    foreach (var item in activeState.Trace)
                    {
                        if (item.Definition.Parent != null)
                        {
                            if (item.Definition.StateKind == StateKind.Initial)
                            {
                                if (builder.Length > 0 && (builder[builder.Length - 1] == ')' || builder[builder.Length - 1] == '\''))
                                {
                                    builder.Append(" ");
                                }

                                builder.Append(item.Definition.Parent.Type.GetGrammarId()).Append("(");
                            }
                            else if (item.Definition.StateKind == StateKind.Custom && GrammarAttributes.Morpheme.IsFreeMorpheme(item.Definition.Value.Type))
                            {
                                builder.Append("'").Append(item.ByTrigger.Value).Append("'");
                            }
                            else if (item.Definition.StateKind == StateKind.Final)
                            {
                                builder.Append(")");
                            }
                        }
                    }

                    builder.AppendLine();
                }

                return builder.ToString();
            }
        }


        public void Add(IWord word)
        {
            myMachine.Fire(word);
        }

        public void Reset() => myMachine.Reset();

        public IEnumerable<IText> GetTexts()
        {
            var result = new List<IText>();

            var linguisticStructureFactory = new LinguisticStructureFactory();

            var relevantActiveStates = myMachine.GetActiveStates().Where(x => x.IsCompleted);

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
                            var structure = linguisticStructureFactory.Create(state.Definition.Parent.Type);

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
