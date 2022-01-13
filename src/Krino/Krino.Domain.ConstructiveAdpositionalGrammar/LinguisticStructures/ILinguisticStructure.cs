﻿using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ILinguisticStructure
    {
        BigInteger Attributes { get; }

        string Value { get; }
    }
}
