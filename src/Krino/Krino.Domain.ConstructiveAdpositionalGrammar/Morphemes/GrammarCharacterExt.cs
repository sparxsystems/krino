using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural;
using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Utility and extenstion functionality for the GrammarCharacter enum.
    /// </summary>
    public static class GrammarCharacterExt
    {
        /// <summary>
        /// Returns all enums of GrammarCharacter.
        /// </summary>
        /// <returns></returns>
        public static GrammarCharacter[] GetValues()
        {
            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();
            return grammarCharacters;
        }

        /// <summary>
        /// Returns the grammar character encoded in the provided attributes.
        /// </summary>
        /// <remarks>
        /// If multiple grammar characters are encoded then it returns the first one.
        /// </remarks>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static GrammarCharacter GetGrammarCharacter(BigInteger attributes)
        {
            if (Attributes.O.IsIn(attributes))
            {
                return GrammarCharacter.O;
            }
            if (Attributes.I.IsIn(attributes))
            {
                return GrammarCharacter.I;
            }
            if (Attributes.A.IsIn(attributes))
            {
                return GrammarCharacter.A;
            }
            if (Attributes.E.IsIn(attributes))
            {
                return GrammarCharacter.E;
            }
            if (Attributes.U.IsIn(attributes))
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
        public static BigInteger GetAttributes(this GrammarCharacter grammarCharacter)
        {
            if (grammarCharacter == GrammarCharacter.O)
            {
                return Attributes.O;
            }
            if (grammarCharacter == GrammarCharacter.I)
            {
                return Attributes.I;
            }
            if (grammarCharacter == GrammarCharacter.A)
            {
                return Attributes.A;
            }
            if (grammarCharacter == GrammarCharacter.E)
            {
                return Attributes.E;
            }
            if (grammarCharacter == GrammarCharacter.U)
            {
                return Attributes.U;
            }

            return Attributes.Epsilon;
        }
    }
}
