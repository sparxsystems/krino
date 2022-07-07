﻿using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
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