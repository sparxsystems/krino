using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Rule evaluating morphemes.
    /// </summary>
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule}")]
    public class MorphemeRule : RuleBase<Morpheme>, IRule<Morpheme>
    {
        public static MorphemeRule Anything => new MorphemeRule(null, MorphRules.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(null, MorphRules.Nothing, MaskRule.Nothing);

        public static MorphemeRule Epsilon => new MorphemeRule(null, MorphRules.EmptyString, MaskRule.Is(0));

        public static MorphemeRule Is(IAttributesModel attributesModel, string morph, BigInteger attributes, IRule<GrammarCharacter> morphematicAdPositionRule = null) => Is(attributesModel, MorphRules.Is(morph), attributes, 0, morphematicAdPositionRule);
        public static MorphemeRule Is(IAttributesModel attributesModel, IRule<string> morphRule, BigInteger attributes, IRule<GrammarCharacter> morphematicAdPositionRule = null) => Is(attributesModel, morphRule, attributes, 0, morphematicAdPositionRule);
        public static MorphemeRule Is(IAttributesModel attributesModel, IRule<string> morphRule, BigInteger attributes, BigInteger notAttributes, IRule<GrammarCharacter> morphematicAdPositionRule = null)
            => notAttributes != 0 ?
                new MorphemeRule(attributesModel, morphRule, MaskRule.Is(attributes) & !MaskRule.Is(notAttributes), morphematicAdPositionRule) :
                new MorphemeRule(attributesModel, morphRule, MaskRule.Is(attributes), morphematicAdPositionRule);

        public static MorphemeRule O_Lexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.O_Lexeme));

        public static MorphemeRule O_Lexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.O_Lexeme));


        public static MorphemeRule I_Lexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.I_Lexeme));

        public static MorphemeRule I_Lexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.I_Lexeme));

        public static MorphemeRule A_Lexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.A_Lexeme));
        public static MorphemeRule A_Lexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.A_Lexeme));
        public static MorphemeRule A_Lexeme_Adjective_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.A_Lexeme_Adjective));
        public static MorphemeRule A_Lexeme_Adjective_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.A_Lexeme_Adjective));

        public static MorphemeRule E_Lexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.E_Lexeme));
        public static MorphemeRule E_Lexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.E_Lexeme));


        public static MorphemeRule U_Lexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.U_Lexeme));
        public static MorphemeRule U_Lexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.U_Lexeme));
        public static MorphemeRule U_NonLexeme_Something(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Something, MaskRule.Is(attributesModel.U_NonLexeme));
        public static MorphemeRule U_NonLexeme_Anything(IAttributesModel attributesModel) => new MorphemeRule(attributesModel, MorphRules.Anything, MaskRule.Is(attributesModel.U_NonLexeme));



        private IAttributesModel myAttributesModel;


        public MorphemeRule(IAttributesModel attributesModel, IRule<string> morphRule, IRule<BigInteger> attributesRule,
            IRule<GrammarCharacter> morphematicAdPositionRule = null)
        {
            myAttributesModel = attributesModel;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
            MorphematicAdPositionRule = morphematicAdPositionRule ?? MorphematicAdPositionRules.Epsilon_U_E;
        }

        public MorphemeRule(MorphemeRule morphemeRule)
            : this(morphemeRule.myAttributesModel, morphemeRule.MorphRule, morphemeRule.AttributesRule, morphemeRule.MorphematicAdPositionRule)
        {
        }


        /// <summary>
        /// Rule to evaluate the morph.
        /// </summary>
        public IRule<string> MorphRule { get; private set; }

        /// <summary>
        /// Grammar character accepted by the morpheme rule.
        /// </summary>
        public GrammarCharacter GrammarCharacter
        {
            get
            {
                GrammarCharacter result = GrammarCharacter.e;

                if (myAttributesModel != null && AttributesRule is IValueRule<BigInteger> valueRule)
                {
                    result = myAttributesModel.GetGrammarCharacter(valueRule.Value);
                }

                return result;
            }
        }

        /// <summary>
        /// Rule to evaluate morpheme attributes.
        /// </summary>
        public IRule<BigInteger> AttributesRule { get; private set; }

        /// <summary>
        /// Rule to evaluate if themorpheme can be provided via the substitution.
        /// </summary>
        /// <remarks>
        /// E.g. O can be provided via the A-O pattern.
        /// </remarks>
        public IRule<GrammarCharacter> MorphematicAdPositionRule { get; private set; }

        public MorphemeRule SetMorphematicAdPositionRule(IRule<GrammarCharacter> inheritanceRule)
        {
            MorphematicAdPositionRule = inheritanceRule;
            return this;
        }

        

        /// <summary>
        /// Checks if the morpheme matches the rule.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        public override bool Evaluate(Morpheme morpheme)
        {
            bool result = MorphRule.Evaluate(morpheme.Morph) && AttributesRule.Evaluate(morpheme.Attributes);
            return result;
        }

        public override bool Equals(IRule<Morpheme> other) => other is MorphemeRule otherMorphemeRule &&
            MorphRule.Equals(otherMorphemeRule.MorphRule) &&
            AttributesRule.Equals(otherMorphemeRule.AttributesRule) &&
            MorphematicAdPositionRule.Equals(otherMorphemeRule.MorphematicAdPositionRule);


        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ MorphematicAdPositionRule.GetHashCode();

            return hash;
        }

    }
}
