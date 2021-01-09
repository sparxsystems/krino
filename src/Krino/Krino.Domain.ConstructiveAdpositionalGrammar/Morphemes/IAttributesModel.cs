using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public interface IAttributesModel
    {
        BigInteger O { get; }

        BigInteger I { get; }

        BigInteger A { get; }

        BigInteger E { get; }

        BigInteger U { get; }

        BigInteger Epsilon { get; }

        bool IsO(BigInteger attributes);
        bool IsI(BigInteger attributes);
        bool IsA(BigInteger attributes);
        bool IsE(BigInteger attributes);
        bool IsU(BigInteger attributes);

        GrammarCharacter GetGrammarCharacter(BigInteger attributes);

        BigInteger GetAttributes(GrammarCharacter grammarCharacter);

        bool IsVerb(BigInteger attributes);

        bool IsValencySpecified(BigInteger attributes);

        int GetNumberOfValencies(BigInteger attributes);

        bool IsLexeme(BigInteger attributes);

        bool IsNonLexeme(BigInteger attributes);

        bool IsPrefix(BigInteger attributes);

        bool IsSuffix(BigInteger attributes);
    }
}
