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

        public List<IPhraseItem> Items { get; } = new List<IPhraseItem>();

        public string Value => string.Join(" ", Items.Select(x => x.Value));

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", Items.Select(x => x.GrammarStr)), ")");
    }
}
