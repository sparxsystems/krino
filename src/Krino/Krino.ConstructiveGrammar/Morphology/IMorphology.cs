using Krino.ConstructiveGrammar.LinguisticStructures;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Morphology
{
    public interface IMorphology
    {
        string GetValue(IEnumerable<IMorpheme> morphemes);

        BigInteger GetAttributes(IEnumerable<IMorpheme> morphemes);

        IEnumerable<IEnumerable<IMorpheme>> GetDerivationSequence(IEnumerable<IMorpheme> morphemes);

        (IEnumerable<IMorpheme> Prefixes, IEnumerable<IMorpheme> Roots, IEnumerable<IMorpheme> Suffixes) Decompose(IEnumerable<IMorpheme> morphemes);
    }
}
