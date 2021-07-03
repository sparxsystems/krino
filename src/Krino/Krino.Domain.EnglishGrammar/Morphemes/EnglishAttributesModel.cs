using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Morphemes
{
    public class EnglishAttributesModel : IAttributesModel
    {
        public BigInteger O => EnglishAttributes.O;
        public BigInteger O_Lexeme => EnglishAttributes.O.Lexeme;

        public BigInteger I => EnglishAttributes.I;
        public BigInteger I_Lexeme => EnglishAttributes.I.Lexeme;

        public BigInteger A => EnglishAttributes.A;
        public BigInteger A_Lexeme => EnglishAttributes.A.Lexeme;
        public BigInteger A_Lexeme_Adjective => EnglishAttributes.A.Lexeme.Adjective;

        public BigInteger E => EnglishAttributes.E;
        public BigInteger E_Lexeme => EnglishAttributes.E.Lexeme;

        public BigInteger U => EnglishAttributes.U;
        public BigInteger U_Lexeme => EnglishAttributes.U.Lexeme;
        public BigInteger U_NonLexeme => EnglishAttributes.U.NonLexeme;

        public BigInteger Epsilon => EnglishAttributes.Epsilon;


        public bool IsO(BigInteger attributes) => EnglishAttributes.O.IsIn(attributes);


        public bool IsI(BigInteger attributes) => EnglishAttributes.I.IsIn(attributes);


        public bool IsA(BigInteger attributes) => EnglishAttributes.A.IsIn(attributes);

        public bool IsE(BigInteger attributes) => EnglishAttributes.E.IsIn(attributes);


        public bool IsU(BigInteger attributes) => EnglishAttributes.U.IsIn(attributes);

        public GrammarCharacter GetGrammarCharacter(BigInteger attributes)
        {
            if (EnglishAttributes.O.IsIn(attributes))
            {
                return GrammarCharacter.O;
            }
            if (EnglishAttributes.I.IsIn(attributes))
            {
                return GrammarCharacter.I;
            }
            if (EnglishAttributes.A.IsIn(attributes))
            {
                return GrammarCharacter.A;
            }
            if (EnglishAttributes.E.IsIn(attributes))
            {
                return GrammarCharacter.E;
            }
            if (EnglishAttributes.U.IsIn(attributes))
            {
                return GrammarCharacter.U;
            }

            return GrammarCharacter.e;
        }

        /// <summary>
        /// Returns structural attributes for the provided grammar character.
        /// </summary>
        /// <param name="grammarCharacter"></param>
        /// <returns></returns>
        public BigInteger GetAttributes(GrammarCharacter grammarCharacter)
        {
            if (grammarCharacter == GrammarCharacter.O)
            {
                return EnglishAttributes.O;
            }
            if (grammarCharacter == GrammarCharacter.I)
            {
                return EnglishAttributes.I;
            }
            if (grammarCharacter == GrammarCharacter.A)
            {
                return EnglishAttributes.A;
            }
            if (grammarCharacter == GrammarCharacter.E)
            {
                return EnglishAttributes.E;
            }
            if (grammarCharacter == GrammarCharacter.U)
            {
                return EnglishAttributes.U;
            }

            return EnglishAttributes.Epsilon;
        }

        public bool IsVerb(BigInteger attributes) => EnglishAttributes.I.Lexeme.Verb.IsIn(attributes);

        public bool IsConjunction(BigInteger attributes) => EnglishAttributes.U.Lexeme.Conjunction.IsIn(attributes);

        public bool IsSubOrdinatingConjunction(BigInteger attributes) => EnglishAttributes.U.Lexeme.Conjunction.Subordinating.IsIn(attributes);

        public bool IsCoordinatingConjunction(BigInteger attributes) =>
            EnglishAttributes.U.Lexeme.Conjunction.Coordinating.IsIn(attributes) ||
            EnglishAttributes.U.Lexeme.Conjunction.ConjunctiveAdverb.IsIn(attributes) ||
            EnglishAttributes.U.NonLexeme.PunctuationMark.Semicolon.IsIn(attributes);

        public bool IsCauseConjunction(BigInteger attributes) => EnglishAttributes.U.Lexeme.Conjunction.Subordinating.Sememe.Cause.IsIn(attributes);

        /// <summary>
        /// True if the value encodes at least one valancy attribute.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsValencySpecified(BigInteger value)
        {
            bool result = (EnglishAttributes.I.Lexeme.Verb.Valency.Avalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Avalent ||
                (EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent ||
                (EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent ||
                (EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent ||
                (EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent ||
                (EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent & value) == EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent;

            return result;
        }

        /// <summary>
        /// Etract the valency attribute from the value and converts it to the number.
        /// </summary>
        /// <remarks>
        /// If there are encoded several valencies then it returns the lowest one.
        /// </remarks>
        /// <param name="value"></param>
        /// <returns></returns>
        public int GetNumberOfValencies(BigInteger value)
        {
            if (EnglishAttributes.I.Lexeme.Verb.Valency.Pentavalent.IsIn(value))
            {
                return 5;
            }

            if (EnglishAttributes.I.Lexeme.Verb.Valency.Quadrivalent.IsIn(value))
            {
                return 4;
            }

            if (EnglishAttributes.I.Lexeme.Verb.Valency.Trivalent.IsIn(value))
            {
                return 3;
            }

            if (EnglishAttributes.I.Lexeme.Verb.Valency.Bivalent.IsIn(value))
            {
                return 2;
            }

            if (EnglishAttributes.I.Lexeme.Verb.Valency.Monovalent.IsIn(value))
            {
                return 1;
            }

            if (EnglishAttributes.I.Lexeme.Verb.Valency.Avalent.IsIn(value))
            {
                return 0;
            }

            return -1;
        }


        public bool IsLexeme(BigInteger attributes) => EnglishAttributes.IsLexeme(attributes);


        public bool IsNonLexeme(BigInteger attributes) => EnglishAttributes.IsNonLexeme(attributes);

        public bool IsPrefix(BigInteger attributes) => EnglishAttributes.IsPrefix(attributes);

        public bool IsSuffix(BigInteger attributes) => EnglishAttributes.IsSuffix(attributes);

        public IEnumerable<EnumBase> FindParticularAttributes(BigInteger attributes) => EnglishAttributes.FindParticularAttributes(attributes);
    }
}
