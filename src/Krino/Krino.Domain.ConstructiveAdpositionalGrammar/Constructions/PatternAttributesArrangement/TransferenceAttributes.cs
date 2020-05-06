using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement
{
    /// <summary>
    /// Attributes of transference
    /// </summary>
    /// <remarks>
    /// Transference is a type of transformation which changes (updates) the grammar character.
    /// </remarks>
    public class TransferenceAttributes : EnumGroupBase
    {
        public TransferenceAttributes(EnumGroupBase parent) : base(parent)
        {
            Derivation = new EnumValue(this);
            Ablaut = new EnumValue(this);
            Suppletion = new EnumValue(this);
        }

        /// <summary>
        /// E.g. adds a postfix.
        /// </summary>
        public EnumValue Derivation { get; }

        /// <summary>
        /// Systematic variation of vowels e.g. sing, sang, sung.
        /// </summary>
        public EnumValue Ablaut { get; }

        /// <summary>
        /// All morphs are different e.g. go, went, gone
        /// </summary>
        public EnumValue Suppletion { get; }
    }
}
