using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
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

        public string Value => string.Join(" ", DirectItems.Select(x => x.Value));

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", DirectItems.Select(x => x.GrammarStr)), ")");

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
