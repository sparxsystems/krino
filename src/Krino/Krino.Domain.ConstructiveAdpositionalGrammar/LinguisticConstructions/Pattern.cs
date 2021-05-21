﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions
{
    /// <summary>
    /// Defines the pattern which contains rules how adtrees can be connected to each other.
    /// </summary>
    [DebuggerDisplay("{Name}")]
    public class Pattern : IEquatable<Pattern>
    {
        private string myName;

        public Pattern(string name = null, BigInteger attributes = default(BigInteger))
        {
            Attributes = attributes;
            myName = name;
        }

        // Optional information for the debugging purposes.
        public string Name
        {
            get
            {
                if (!string.IsNullOrEmpty(myName))
                {
                    return myName;
                }

                string name;
                if ((LeftRule == null || LeftRule.Equals(MorphemeRule.Nothing)) &&
                    (RightRule == null || RightRule.Equals(MorphemeRule.Nothing)))
                {
                    name = UpRule.GrammarCharacter.ToString();
                }
                else
                {
                    name = string.Join("", LeftRule?.GrammarCharacter.ToString(), "-", UpRule.GrammarCharacter, "-", RightRule?.GrammarCharacter);
                }

                return name;
            }
        }

        public string Description { get; set; }

        public BigInteger Attributes { get; private set; }


        public MorphemeRule UpRule { get; set; } = MorphemeRule.Nothing;

        public BigInteger UpAttributes { get; set; }

        public MorphemeRule LeftRule { get; set; } = MorphemeRule.Nothing;

        public BigInteger LeftAttributes { get; set; }

        public MorphemeRule RightRule { get; set; } = MorphemeRule.Nothing;

        public BigInteger RightAttributes { get; set; }

        /// <summary>
        /// The valency position required by the pattern.
        /// </summary>
        /// <remarks>
        /// If 0 then the valency position is not required.
        /// </remarks>
        public int ValencyPosition { get; set; }

        /// <summary>
        /// Indicates if the left branch is before the right branch in the phrase sequence.
        /// </summary>
        public bool IsLeftFirst { get; set; }

        public Pattern SetLeftFirst()
        {
            IsLeftFirst = true;
            return this;
        }

        public Pattern SetSubstitutionForLeft(IRule<GrammarCharacter> inheritanceRule)
        {
            LeftRule.SetSubstitution(inheritanceRule);
            return this;
        }

        public Pattern SetSubstitutionForRight(IRule<GrammarCharacter> inheritanceRule)
        {
            RightRule.SetSubstitution(inheritanceRule);
            return this;
        }

        public bool IsLikeMorpheme => IsMorpheme || IsMonoTransference | IsPairTransference;

        /// <summary>
        /// Returns true if the pattern represents a morpheme.
        /// </summary>
        /// <returns></returns>
        public bool IsMorpheme
        {
            get
            {
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    UpRule.MorphRule.Equals(MorphRules.Something) &&
                    LeftRule.Equals(MorphemeRule.Nothing) &&
                    RightRule.Equals(MorphemeRule.Nothing))
                {
                    return true;
                }

                return false;
            }
        }

        /// <summary>
        /// Returns true if the pattern uses both positions (left and right) to set the grammar character. 
        /// </summary>
        /// <remarks>
        /// The adtree driven by the pair transference acts as morpheme.
        /// E.g. this pattern can be used to create noun from a verb by the -ing suffix.
        /// </remarks>
        /// <returns></returns>
        public bool IsPairTransference
        {
            get
            {
                // AdPosition
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                    UpRule.MorphRule.Equals(MorphRules.EmptyString))
                {
                    // Left.
                    if (LeftRule.GrammarCharacter != GrammarCharacter.e)
                    {
                        // Right - inheriting site.
                        if (RightRule.GrammarCharacter != GrammarCharacter.e)
                        {
                            return true;
                        }
                    }
                }

                return false;
            }
        }

        /// <summary>
        /// Returs true if the pattern just changes the grammar character of the right position (the left position stays empty).
        /// </summary>
        /// <remarks>
        /// E.g. english nouns can be used as adjectives.
        /// </remarks>
        /// <returns></returns>
        public bool IsMonoTransference
        {
            get
            {
                // AdPosition
                if (UpRule.GrammarCharacter != GrammarCharacter.e &&
                    UpRule.GrammarCharacter != GrammarCharacter.U &&
                    UpRule.AttributesRule is IReferenceValueRule<BigInteger> &&
                    UpRule.MorphRule.Equals(MorphRules.EmptyString))
                {
                    // Left.
                    if (LeftRule.Equals(MorphemeRule.Nothing))
                    {
                        // Right - inheriting site.
                        if (RightRule.GrammarCharacter != GrammarCharacter.e)
                        {
                            return true;
                        }
                    }
                }

                return false;
            }
        }

        public bool IsEpsilonAdPosition()
        {
            bool result = UpRule.Equals(MorphemeRule.Epsilon) &&
                          !RightRule.Equals(MorphemeRule.Nothing) &&
                          !LeftRule.Equals(MorphemeRule.Nothing) &&
                          !RightRule.MorphRule.Evaluate(null) && !RightRule.MorphRule.Evaluate("") && !RightRule.AttributesRule.Evaluate(0) &&
                          !LeftRule.MorphRule.Evaluate(null) && !LeftRule.MorphRule.Evaluate("") && !LeftRule.AttributesRule.Evaluate(0);

            return result;
        }

        /// <summary>
        /// Returns true if the pattern represents an adposition with a morpheme.
        /// </summary>
        /// <remarks>
        /// E.g. an adposition with the U grammar character containing a conjunction.
        /// Or an adposition with the E grammar character containing a preposition.
        /// </remarks>
        /// <returns></returns>
        public bool IsMorphematicAdPosition()
        {
            bool result = UpRule.MorphRule.Equals(MorphRules.Something) &&
                          !UpRule.AttributesRule.Evaluate(0) &&
                          !RightRule.Equals(MorphemeRule.Nothing) &&
                          !LeftRule.Equals(MorphemeRule.Nothing) &&
                          !RightRule.MorphRule.Evaluate(null) && !RightRule.MorphRule.Evaluate("") && !RightRule.AttributesRule.Evaluate(0) &&
                          !LeftRule.MorphRule.Evaluate(null) && !LeftRule.MorphRule.Evaluate("") && !LeftRule.AttributesRule.Evaluate(0);

            return result;
        }


        public bool Equals(Pattern other) =>
            UpRule.Equals(other.UpRule) &&
            LeftRule.Equals(other.LeftRule) &&
            RightRule.Equals(other.RightRule) &&
            ValencyPosition == other.ValencyPosition;


        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ UpRule.GetHashCode();
            hash = (hash * 16777619) ^ LeftRule.GetHashCode();
            hash = (hash * 16777619) ^ RightRule.GetHashCode();
            hash = (hash * 16777619) ^ ValencyPosition.GetHashCode();

            return hash;
        }
    }
}