using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal abstract class PhraseBase : LinguisticStructureBase, IPhrase
    {
        public PhraseBase(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }

        public List<IPhraseItem> Items { get; } = new List<IPhraseItem>();

        public string Value => string.Join(" ", Items.Select(x => x.Value));
    }
}
