using Krino.ConstructiveGrammar.LinguisticStructures;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Morphology
{
    public interface IMorphology
    {
        string GetValue(IEnumerable<IMorpheme> morphemes);

        BigInteger GetAttributes(IEnumerable<IMorpheme> morphemes);
    }
}
