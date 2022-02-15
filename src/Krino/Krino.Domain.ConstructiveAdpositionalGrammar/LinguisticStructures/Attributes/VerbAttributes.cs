﻿using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Verb attributes.
    /// </summary>
    public class VerbAttributes : EnumGroupBase
    {
        public VerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Modal = new EnumValue(this);
            Regular = new EnumValue(this);
            Irregular = new EnumValue(this);
            Stative = new StativeVerbAttributes(this);
            Action = new ActiveVerbAttributes(this);
            Auxiliary = new EnumValue(this);
            InfinitiveMarker = new EnumValue(this);
            Form = new VerbFormAttributes(this);
            Valency = new VerbValencyAttributes(this);
            Sememe = new VerbSememes(this);
        }

        /// <summary>
        /// The verb is modal. e.g. may, can, will.
        /// </summary>
        public EnumValue Modal { get; }

        /// <summary>
        /// Regular verb.
        /// </summary>
        public EnumValue Regular { get; }

        /// <summary>
        /// Irregular verb.
        /// </summary>
        public EnumValue Irregular { get; }

        /// <summary>
        /// Stative verb.
        /// </summary>
        /// <remarks>
        /// A stative verb defines the state of something: examples are sit, lie, stand.
        /// </remarks>
        public StativeVerbAttributes Stative { get; }

        /// <summary>
        /// Action (or dynamic) verb.
        /// </summary>
        public ActiveVerbAttributes Action { get; }

        /// <summary>
        /// Helper verbs e.g. will.
        /// </summary>
        public EnumValue Auxiliary { get; }

        /// <summary>
        /// 'to' which marks the infinitive.
        /// </summary>
        public EnumValue InfinitiveMarker { get; }

        /// <summary>
        /// Form of the verb.
        /// </summary>
        public VerbFormAttributes Form { get; }

        /// <summary>
        /// Valency of the verb.
        /// </summary>
        public VerbValencyAttributes Valency { get; }

        /// <summary>
        /// Semantic attributes.
        /// </summary>
        public VerbSememes Sememe { get; }
    }
}