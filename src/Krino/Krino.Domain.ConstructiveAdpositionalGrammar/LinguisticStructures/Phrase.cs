using System.Numerics;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    internal class Phrase : PhraseBase, IPhrase
    {
        public Phrase(BigInteger attributes)
            : base(attributes)
        {
        }

        protected override PhraseBase FactoryMethod() => new Phrase(Attributes);
    }
}
