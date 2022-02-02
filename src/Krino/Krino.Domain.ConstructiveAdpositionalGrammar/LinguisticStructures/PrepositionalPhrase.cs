using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class PrepositionalPhrase : PhraseBase, IPrepositionalPhrase, IPhrase
    {
        public PrepositionalPhrase(BigInteger attributes)
            : base(attributes)
        {
        }

        public IWord Preposition { get; set; }

        public INounElement ObjectOfPreposition { get; set; }
    }
}
