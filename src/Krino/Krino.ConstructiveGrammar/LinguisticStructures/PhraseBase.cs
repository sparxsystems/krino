using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    internal abstract class PhraseBase : LinguisticStructureBase, IPhrase
    {
        public PhraseBase(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<IPhraseItem> DirectItems { get; } = new List<IPhraseItem>();

        public IEnumerable<IPhraseItem> AllItems
        {
            get
            {
                var stack = new Stack<IPhraseItem>();
                stack.Push(this);

                while (stack.Count > 0)
                {
                    var aThis = stack.Pop();
                    yield return aThis;

                    if (aThis is IPhrase aThisPhrase)
                    {
                        foreach (var child in aThisPhrase.DirectItems.AsEnumerable().Reverse())
                        {
                            stack.Push(child);
                        }
                    }
                }
            }
        }

        public string Value => string.Join(" ", DirectItems.Select(x => x.Value)).Trim();

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", DirectItems.Select(x => x.GrammarStr)), ")");

        public void BuildFormattedGrammarStr(int indent, StringBuilder builder)
        {
            if (!GrammarAttributes.Item.IsIn(Attributes))
            {
                builder.Append(new string(' ', indent)).Append(Value).Append(" : ").AppendLine(AttributesStr);
                DirectItems.ForEach(x => x.BuildFormattedGrammarStr(indent + 4, builder));
            }
            else
            {
                // As if jump over if it is an item within a compound.
                DirectItems.ForEach(x => x.BuildFormattedGrammarStr(indent, builder));
            }
        }

        public ILinguisticStructure DeepCopy()
        {
            var result = FactoryMethod();
            var items = DirectItems.Select(x => x.DeepCopy()).OfType<IPhraseItem>();
            result.DirectItems.AddRange(items);
            return result;
        }

        protected abstract PhraseBase FactoryMethod();
    }
}
