using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.StateMachines;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class GrammarMachine
    {
        private MultiMachine<LinguisticState, IWord> myMachine;

        public GrammarMachine(MultiMachine<LinguisticState, IWord> grammarRules)
        {
            myMachine = grammarRules;
        }

        public void Add(IWord word)
        {
            myMachine.Fire(word);
        }

        public void Reset() => myMachine.Reset();

        public bool IsActive => myMachine.IsActive;

        public IEnumerable<StatePath<LinguisticState, IWord>> ActiveStates => myMachine.GetActiveStates();

        public IEnumerable<IText> GetTexts()
        {
            var result = new List<IText>();

            var relevantActiveStates = ActiveStates.Where(x => x.IsCompleted);

            foreach (var activeState in relevantActiveStates)
            {
                // Note: the grammar machine top-most state machine consists of one Text state.
                var text = activeState.Path.GetText();
                result.Add(text);
            }

            return result;
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

                    foreach (var item in activeState.Path)
                    {
                        if (item.Definition.Parent != null)
                        {
                            if (item.Definition.StateKind == StateKind.Initial)
                            {
                                if (builder.Length > 0 && (builder[builder.Length - 1] == ')' || builder[builder.Length - 1] == '\''))
                                {
                                    builder.Append(" ");
                                }

                                builder.Append(item.Definition.Parent.Attributes.GetGrammarId()).Append("(");
                            }
                            else if (item.Definition.StateKind == StateKind.Custom && GrammarAttributes.Morpheme.Free.IsIn(item.Definition.Value.Attributes))
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
    }
}
