using System;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
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
            if (StructuralAttributes.O.IsIn(attributes))
            {
                return GrammarCharacter.O;
            }
            if (StructuralAttributes.I.IsIn(attributes))
            {
                return GrammarCharacter.I;
            }
            if (StructuralAttributes.A.IsIn(attributes))
            {
                return GrammarCharacter.A;
            }
            if (StructuralAttributes.E.IsIn(attributes))
            {
                return GrammarCharacter.E;
            }
            if (StructuralAttributes.U.IsIn(attributes))
            {
                return GrammarCharacter.U;
            }

            return GrammarCharacter.Epsilon;
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
                return StructuralAttributes.O;
            }
            if (grammarCharacter == GrammarCharacter.I)
            {
                return StructuralAttributes.I;
            }
            if (grammarCharacter == GrammarCharacter.A)
            {
                return StructuralAttributes.A;
            }
            if (grammarCharacter == GrammarCharacter.E)
            {
                return StructuralAttributes.E;
            }
            if (grammarCharacter == GrammarCharacter.U)
            {
                return StructuralAttributes.U;
            }

            return StructuralAttributes.Epsilon;
        }
    }
}
