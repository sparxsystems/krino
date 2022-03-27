﻿using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class VerbElement : VerbElementBase, IVerbElement
    {
        public VerbElement(BigInteger attributes) : base(attributes)
        {
        }

        protected override PhraseBase FactoryMethod() => new VerbElement(Attributes);
    }
}
