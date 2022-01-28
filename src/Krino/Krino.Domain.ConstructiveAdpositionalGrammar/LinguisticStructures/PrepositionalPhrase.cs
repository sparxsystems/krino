using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class PrepositionalPhrase : PhraseBase, IPrepositionalPhrase, IPhrase
    {
        public PrepositionalPhrase(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }

        public IWord Preposition { get; set; }

        public INounElement ObjectOfPreposition { get; set; }
    }
}
