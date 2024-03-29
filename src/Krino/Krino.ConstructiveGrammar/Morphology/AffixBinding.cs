﻿using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;
using Krino.Vertical.Utils.Diagnostic;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using System;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Morphology
{
    public class AffixBinding : IMorphemeBinding
    {
        public static AffixBinding Prefix(string prefix) => new AffixBinding() { TransformValue = Trans.Prepend(prefix) };
        public static AffixBinding Suffix(string suffix) => new AffixBinding() { TransformValue = Trans.Append(suffix) };

        public BigInteger AttributesToPick { get; init; }

        public BigInteger AttributesToDrop { get; init; }

        public IRule<IWord> CanBindRule { get; init; } = RuleMaker.Anything<IWord>();

        public ITransformation<string> TransformValue { get; init; } = Trans.NothingToDo<string>();


        bool IMorphemeBinding.CanBind(IWord word) => CanBindRule.Evaluate(word);

        string IMorphemeBinding.TransformValue(string word) => TransformValue.Transform(word);


        public BigInteger TransformAttributes(BigInteger wordAttributes)
        {
            using var _t = Trace.Entering();

            BigInteger result = wordAttributes;

            var enumsToRemove = GrammarAttributes.Instance.FindEnums(AttributesToDrop);
            foreach (var enumToRemove in enumsToRemove)
            {
                if (enumToRemove is EnumValue enumValueToRemove)
                {
                    result = enumValueToRemove.Clear(result);
                }
                else if (enumToRemove is EnumGroupBase enumGroupToRemove)
                {
                    result = enumGroupToRemove.Clear(result);
                }
            }

            result |= AttributesToPick;

            return result;
        }



        public bool Equals(IMorphemeBinding other)
            => other is AffixBinding otherBinding &&
               (CanBindRule == otherBinding.CanBindRule || CanBindRule != null && otherBinding.CanBindRule != null && CanBindRule.Equals(otherBinding.CanBindRule)) &&
               AttributesToPick == otherBinding.AttributesToPick && AttributesToDrop == otherBinding.AttributesToDrop &&
               (TransformValue == otherBinding.TransformValue || TransformValue != null && otherBinding.TransformValue != null && TransformValue.Equals(otherBinding.TransformValue));


        public override bool Equals(object obj) => obj is AffixBinding other && Equals(other);

        public static bool operator ==(AffixBinding a, AffixBinding b) => a.Equals(b);

        public static bool operator !=(AffixBinding a, AffixBinding b) => !(a == b);

        public override int GetHashCode() => HashCode.Combine(AttributesToPick.GetHashCode(), AttributesToDrop.GetHashCode(), CanBindRule.GetHashCode());

        
    }
}
