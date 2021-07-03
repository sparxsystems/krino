using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public interface IAttributesModel
    {
        BigInteger O { get; }
        BigInteger O_Lexeme { get; }

        BigInteger I { get; }
        BigInteger I_Lexeme { get; }

        BigInteger A { get; }
        BigInteger A_Lexeme { get; }
        BigInteger A_Lexeme_Adjective { get; }

        BigInteger E { get; }
        BigInteger E_Lexeme { get; }

        BigInteger U { get; }
        BigInteger U_Lexeme { get; }
        BigInteger U_NonLexeme { get; }

        BigInteger Epsilon { get; }

        bool IsO(BigInteger attributes);
        bool IsI(BigInteger attributes);
        bool IsA(BigInteger attributes);
        bool IsE(BigInteger attributes);
        bool IsU(BigInteger attributes);

        GrammarCharacter GetGrammarCharacter(BigInteger attributes);

        BigInteger GetAttributes(GrammarCharacter grammarCharacter);

        bool IsVerb(BigInteger attributes);

        bool IsConjunction(BigInteger attributes);

        bool IsSubOrdinatingConjunction(BigInteger attributes);

        bool IsCoordinatingConjunction(BigInteger attributes);

        bool IsCauseConjunction(BigInteger attributes);


        bool IsValencySpecified(BigInteger attributes);

        int GetNumberOfValencies(BigInteger attributes);

        bool IsLexeme(BigInteger attributes);

        bool IsNonLexeme(BigInteger attributes);

        bool IsPrefix(BigInteger attributes);

        bool IsSuffix(BigInteger attributes);

        IEnumerable<EnumBase> FindParticularAttributes(BigInteger attributes);
    }
}
