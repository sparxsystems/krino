using System;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IMorphemeBinding : IEquatable<IMorphemeBinding>
    {
        BigInteger AttributesToPick { get; }

        bool CanBind(IWord word);

        string TransformValue(string word);

        BigInteger TransformAttributes(BigInteger wordAttributes);
    }
}
