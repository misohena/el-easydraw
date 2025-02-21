;;; edraw-msg-ja.el --- Japanese Message Catalog    -*- lexical-binding: t; -*-

;; Copyright (C) 2023 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar edraw-msg-hash-table nil)

(setq
 edraw-msg-hash-table
 #s(hash-table
    test equal
    data
    (
     ;; (load-library (expand-file-name "./edraw-msg-tools.el"))
     ;; (edraw-msg-update-catalog-buffer)
     ;; M-x edraw-msg-search at point
     ;; [BEGIN MSG DATA]
     "%s (%g,%g)-(%g,%g) selected" "%s (%g,%g)-(%g,%g)を選択中"
     "%s Selected Shapes" "%s個の選択図形"
     "%s Source Code: " "%s ソースコード: "
     "%s objects (%g,%g)-(%g,%g) selected" "%s個のオブジェクト(%g,%g)-(%g,%g)を選択中"
     "%s objects selected" "%s個のオブジェクトを選択中"
     "%s shapes" "%s個の図形"
     "%s selected" "%sを選択中"
     "(Initial %s Marker Default)" "(編集開始時の%sマーカーのデフォルト)"
     "(Initial %s Shape Default)" "(編集開始時の%s図形のデフォルト)"
     "(Initial %s Tool Default)" "(編集開始時の%sツールのデフォルト)"
     "<no name>" "<無名>"
     "Actions" "操作"
     "Already an SVG for Edraw" "既にedraw用のSVGです"
     "Already glued" "すでに接着されています"
     "Amplitude: " "振幅: "
     "Anchor (%g,%g) selected" "アンカー(%g,%g)を選択中"
     "Anchor Points" "アンカーポイント"
     "Anchor" "アンカー"
     "Angle: " "角度: "
     "Apply group's transform property to children?" "グループのtransformプロパティをグループ内に適用しますか?"
     "Apply transform property to anchors" "transformプロパティをアンカーポイントへ適用"
     "Apply" "適用"
     "Arrow" "矢印"
     "Aspect Ratio: " "縦横比"
     "Auto" "自動"
     "Background Color: " "背景色: "
     "Blue..." "青..."
     "Blue[0~255|0.0~1.0]: " "青[0~255|0.0~1.0]: "
     "Brightness..." "明度..."
     "Bring Forward" "手前へ"
     "Brightness[0.0~1.0]: " "明るさ[0.0~1.0]: "
     "Bring to Front" "最前面へ"
     "CSS Style parsing error: %s" "CSSスタイル解析エラー: %s"
     "Cancel Edit" "編集をキャンセル"
     "Cannot glue because it would create a circular reference" "循環参照になるため接着できません"
     "Change Text: " "テキスト変更: "
     "Change to Current Color" "現在の色に変更"
     "Child Frame" "子フレーム"
     "Choose" "選択"
     "Circle" "円"
     "Clear All States" "全状態をクリア"
     "Clear Temporary States" "一時状態をクリア"
     "Clear..." "クリア..."
     "Click:" "クリック:"
     "Close Path" "パスを閉じる"
     "Close Related Windows" "関連ウィンドウを閉じる"
     "Close" "閉じる"
     "Closed" "閉じました"
     "Coil Line" "コイル線"
     "Color Components" "色成分"
     "Color Name Usage" "色名の使用"
     "Color Name..." "色名..."
     "Color Picker Menu" "カラーピッカーメニュー"
     "Color Name: " "色名: "
     "Color" "色"
     "Combine Paths" "パスの結合"
     "Connected" "接続しました"
     "Continue" "継続"
     "Convert To Path" "パスへ変換"
     "Convert To [[edraw:data=]]" "[[edraw:data=]]形式へ変換"
     "Convert To [[edraw:file=]]" "[[edraw:file=]]形式へ変換"
     "Convert To [[file:]]" "[[file:]]形式へ変換"
     "Convert contents back to text format? " "バッファの内容をテキスト形式に戻しますか?"
     "Copied %s entries" "%s項目をコピーしました"
     "Copied %s" "%sをコピーしました"
     "Copy Contents" "内容をコピー"
     "Copy" "コピー"
     "Crop..." "切り抜き..."
     "Custom Shape Tool" "カスタムシェイプツール"
     "Custom shapes have unsaved changes." "カスタムシェイプに未保存の変更があります"
     "Cut %s entries" "%s項目をカットしました"
     "Cut %s" "%sをカットしました"
     "Cut" "カット"
     "Decrease Opacity by %d" "不透明度を%d減らす"
     "Default Config" "デフォルト設定"
     "Decrease X by %d" "Xを%d減らす"
     "Defaults" "デフォルト"
     "Decrease Y by %d" "Yを%d減らす"
     "Delete Point" "点を削除"
     "Decrease Z by %d" "Zを%d減らす"
     "Delete Preset" "プリセット削除"
     "Delete" "削除"
     "Delete..." "削除..."
     "Delta X: " "X移動量: "
     "Delta Y: " "Y移動量: "
     "Deselect All" "全選択解除"
     "Deselect" "選択解除"
     "Direction(degrees): " "方向(度): "
     "Disabled" "不許可"
     "Discard `use' element with unsupported format" "未対応形式の`use'要素を破棄"
     "Discard changes?" "変更を破棄しますか?"
     "Discard unsupported attribute: %s" "未対応の属性を破棄: %s"
     "Do you want to close the current document?" "現在のドキュメントを閉じますか?"
     "Discard unsupported element: %s" "未対応の要素を破棄: %s"
     "Do you want to delete all presets?" "全てのプリセットを削除しますか?"
     "Do you want to overwrite?" "上書きしますか?"
     "Do you want to restore the palette to its initial state?" "パレットを初期状態へ戻しますか?"
     "Document Height [px|%]: " "ドキュメント高さ [px|%]: "
     "Document Quick" "ドキュメント クイック"
     "Document Width [px|%]: " "ドキュメント幅 [px|%]: "
     "Document" "ドキュメント"
     "Drag the cropping range." "切り抜き範囲をドラッグで指定してください。"
     "Drag the grid creation range" "グリッドを作成する範囲をドラッグしてください"
     "Duplicate" "複製"
     "Duplicated %s shapes" "%s個の図形を複製"
     "Edit" "編集"
     "Edraw editor has unsaved changes. Discard changes ?" "エディタには未保存の変更があります。変更を破棄しますか?"
     "Ellipse Tool" "楕円ツール"
     "Ellipse" "楕円"
     "Empty SVG data" "空のSVGデータ"
     "Empty `points' attribute: %s" "属性`points'が空: %s"
     "Empty path data" "空のパスデータ"
     "Empty shape" "空の図形"
     "Empty shapes cannot be registered" "空の図形は登録できません"
     "Enabled" "許可"
     "End Marker" "終点マーカー"
     "Evaluate this generator's code on your system?" "あなたのシステムでこのジェネレータのコードを評価しますか?"
     "Export SVG" "SVGをエクスポート"
     "Export Section" "セクションをエクスポート"
     "Export to Buffer" "バッファへ書き出し"
     "Export to File..." "ファイルへ書き出し..."
     "Failed to delete entry" "項目の削除に失敗しました"
     "Failed to find insertion point" "挿入場所の特定に失敗しました"
     "Failed to get image size" "画像サイズの取得に失敗しました"
     "Failed to save. %s. Discard changes?" "保存に失敗しました。変更を破棄しますか?"
     "File `%s' exists; overwrite? " "ファイル `%s' はすでに存在します。上書きしますか?"
     "File does not exist" "ファイルが存在しません"
     "Fill" "塗り"
     "Fill..." "塗り..."
     "Find File" "ファイルを開く"
     "Finish Edit" "編集終了"
     "Fold All Sections" "全セクション折りたたみ"
     "Font Size..." "フォントサイズ..."
     "Font Size: " "フォントサイズ: "
     "Formatting" "書式"
     "Frame" "フレーム"
     "Freehand Tool" "手書きツール"
     "Generate Shape Along Path" "パスに沿った図形を生成"
     "Generation error: %s" "生成エラー: %s"
     "Glue Point: " "接着点: "
     "Glue Position" "接着位置"
     "Glue position type" "接着位置タイプ"
     "Glue to selected or overlapped shape" "選択または重なり図形と接着"
     "Generator Tool" "生成ツール"
     "Glue" "接着"
     "Glued Text: " "接着テキスト: "
     "Green..." "緑..."
     "Green[0~255|0.0~1.0]: " "緑[0~255|0.0~1.0]: "
     "Grid Interval: " "グリッド間隔: "
     "Grid" "グリッド"
     "Group" "グループ化"
     "HEX" "HEX"
     "Handle (%g,%g) selected" "ハンドル(%g,%g)を選択中"
     "HSL" "HSL"
     "Handle" "ハンドル"
     "HWB" "HWB"
     "Href..." "Href..."
     "History" "履歴"
     "Hue[deg]: " "色相[度]: "
     "Hue..." "色相..."
     "ID `%s' is already in use, so remove it" "ID`%s'はすでに使われているので除去します"
     "If you import diagrams generated with other software into Edraw, they may not be displayed correctly or the editing operation may become unstable. The original information is lost in the converted data. Do you want to convert to a format for Edraw?" "他のソフトウェアで生成した図をedrawで読み込んだ場合、正しく表示されなかったり、編集動作が不安定になる場合があります。変換後のデータからは元の情報が失われます。edrawのための形式へ変換しますか？"
     "Image File: " "画像ファイル: "
     "Image Tool" "画像ツール"
     "Import Section Before" "この前にセクションをインポート"
     "Import Section" "セクションをインポート"
     "Import from File..." "ファイルからインポート..."
     "Increase Opacity by %d" "不透明度を%d増やす"
     "Input name: " "名前入力: "
     "Increase X by %d" "Xを%d増やす"
     "Insert New Section Before" "この前に新しいセクションを挿入"
     "Increase Y by %d" "Yを%d増やす"
     "Insert New Section" "新しいセクションを挿入"
     "Increase Z by %d" "Zを%d増やす"
     "Insert New Shape Before" "この前に新しい図形を挿入"
     "Increase/Decrease" "増減"
     "Insert New Shape" "新しい図形を挿入"
     "Insert Point Before" "この前に点を追加"
     "Invalid number" "無効な数値"
     "Invalid value" "無効な値"
     "Invalid view size" "無効なビューサイズ"
     "Invisible" "非表示"
     "Keep unsupported attribute: %s" "未対応の属性を維持: %s"
     "LAB" "LAB"
     "Link at point does not contain valid data" "この場所のリンクに有効なデータが含まれていません"
     "Keep unsupported element: %s" "未対応の要素を維持: %s"
     "LCH" "LCH"
     "Load Preset" "プリセット読み込み"
     "Load colors from file: " "色の読み込み元ファイル: "
     "Load..." "読み込み..."
     "Main Menu" "メインメニュー"
     "Make Corner" "角にする"
     "Make Smooth" "滑らかにする"
     "Marker Type" "マーカータイプ"
     "Marker" "マーカー"
     "Marker type: " "マーカータイプ: "
     "Menu" "メニュー"
     "Mode Line" "モードライン表示"
     "More..." "その他..."
     "Move Backward Same Level" "同じ階層の後ろへ移動"
     "Move Backward" "後ろへ移動"
     "Move Forward Same Level" "同じ階層の前へ移動"
     "Move Forward" "前へ移動"
     "Move by Coordinates..." "座標による移動..."
     "Moved anchor to (%s,%s)" "アンカーを(%s,%s)へ移動"
     "Moving Distance: " "移動距離: "
     "Moved by (%s,%s)" "移動量:(%s,%s)"
     "Moved handle to (%s,%s)" "ハンドルを(%s,%s)へ移動"
     "Next Color" "次の色"
     "Next Type" "次の種類へ変更"
     "Next" "次"
     "No `points' attribute: %s" "属性`points'がありません: %s"
     "No editor here" "ここにエディタはありません"
     "No entries at point" "この場所に項目がありません"
     "No glue target" "接着先がありません"
     "No group selected" "グループが選択されていません"
     "No link at point" "この場所にリンクがありません"
     "No need to convert" "変換の必要がありません"
     "No need to rotate" "回転の必要がありません"
     "No need to scale" "拡大縮小の必要がありません"
     "No objects selected" "選択オブジェクトなし"
     "No path selected" "パスが選択されていません"
     "No redo data" "やり直しデータがありません"
     "No shape selected" "図形が選択されていません"
     "No shapes" "図形がありません"
     "No target object" "対象オブジェクト無し"
     "No undo data" "取り消しデータがありません"
     "No" "いいえ"
     "None" "なし"
     "Not SVG data" "SVGデータではありません"
     "Not SVG for Edraw" "edraw用のSVGではありません"
     "Not a number" "数値ではない"
     "OKLAB" "OKLAB"
     "Opacity[0.0~1.0]: " "不透明度[0.0~1.0]: "
     "OKLCH" "OKLCH"
     "Open Path" "パスを開く"
     "Opacity..." "不透明度..."
     "Origin X(left, center, right, <percentage>%, or <coordinate>): " "原点X(left, center, right, <百分率>%, or <座標値>): "
     "Output Format (CSS)" "出力書式(CSS)"
     "Overwrite Preset" "プリセットの上書き"
     "Output Format (Emacs)" "出力書式(Emacs)"
     "Overwrite?" "上書きしますか?"
     "Origin X: " "原点X: "
     "Overwrite..." "上書き..."
     "Palette Color #%d" "パレット色#%d"
     "Palette" "パレット"
     "Parsing error: %s" "解析エラー: %s"
     "Paste Before" "直前にペースト"
     "Origin Y(top, center, bottom, <percentage>%, or <coordinate>): " "原点Y(top, center, bottom, <百分率>%, または <座標値>): "
     "Paste" "ペースト"
     "Origin Y: " "原点Y: "
     "Path Tool" "パスツール"
     "Path data does not start with M" "パスデータがMで始まっていません"
     "Path" "パス"
     "Please enter a integer or empty." "整数か空を入力してください"
     "Please enter a integer." "整数を入力してください"
     "Please enter a number or empty." "数値か空を入力してください"
     "Please enter a number, %s, or empty." "数値か%s、または空を入力してください"
     "Please enter a number." "数値を入力してください"
     "Pointer Input Disabled" "ポインター入力無効"
     "Preset %s exists. Do you want to overwrite?" "プリセット %s は存在しています。上書きしますか?"
     "Preset" "プリセット"
     "Prev" "前"
     "Previous Color" "前の色"
     "Properties of %s" "%sのプロパティ一覧"
     "Properties..." "プロパティ一覧..."
     "Property Editor" "プロパティエディタ"
     "Property(Empty:End): " "プロパティ(空:指定終了): "
     "Property: " "プロパティ: "
     "RGB" "RGB"
     "Range:" "範囲:"
     "Recent Color #%d" "最近使った色#%d"
     "Rect Tool" "矩形ツール"
     "Rect" "矩形"
     "Red..." "赤..."
     "Red[0~255|0.0~1.0]: " "赤[0~255|0.0~1.0]: "
     "Redo" "やり直し"
     "Reference Point: " "基準点: "
     "Regenerate" "再生成"
     "Rename Preset" "プリセット改名"
     "Rename" "改名"
     "Rename preset %s to: " "プリセット %s を次の名前に改名: "
     "Rename..." "改名..."
     "Reset Scroll and Zoom" "スクロールとズームをリセット"
     "Reset View" "表示をリセット"
     "Reset to Default" "デフォルトに戻す"
     "Resize..." "リサイズ..."
     "Reverse Path Direction" "パスの向きを反転"
     "Rotate %.2fdeg" "回転 %.2f度"
     "Rotate All..." "全回転..."
     "Rotate..." "回転..."
     "SVG viewBox ([<min-x> <min-y> <width> <height>] or empty): " "SVG viewBox ([<min-x> <min-y> <width> <height>] or 空): "
     "Saturation..." "彩度..."
     "Saturation[0.0~1.0]: " "彩度[0.0~1.0]: "
     "Save as Initial %s Default" "編集開始時の%sのデフォルトとして保存"
     "Save as Initial %s Marker Default" "編集開始時の%sマーカーのデフォルトとして保存"
     "Save as Initial %s Shape Default" "編集開始時の%s図形のデフォルトとして保存"
     "Save colors to file: " "色の保存先ファイル: "
     "Save preset named: " "プリセットを次の名前で保存: "
     "Save..." "保存..."
     "Save" "保存"
     "Scale %.2f%% %.2f%%" "拡大率 %.2f%% %.2f%%"
     "Scale All..." "全拡大縮小..."
     "Scale X [px|%]: " "X拡大率 [px|%]: "
     "Scale..." "拡大縮小..."
     "Scale Y [px|%]: " "Y拡大率 [px|%]: "
     "Scroll and Zoom" "スクロールとズーム"
     "Search Object" "オブジェクトの検索"
     "Select %s" "%sを選択"
     "Select All" "全選択"
     "Select Next Above" "一つ手前を選択"
     "Select Next Below" "一つ奥を選択"
     "Select Tool" "選択ツール"
     "Select an object" "図形を一つ選択してください"
     "Select generator type" "生成タイプを選択してください"
     "Select" "選択"
     "Selected Object" "選択オブジェクト"
     "Send Backward" "後へ"
     "Send to Back" "最背面へ"
     "Set Background..." "背景設定..."
     "Set Glue Position..." "接着位置設定..."
     "Set Grid Interval..." "グリッド間隔設定..."
     "Set Marker..." "マーカー設定..."
     "Set Property" "プロパティ設定"
     "Set View Size..." "表示サイズ設定..."
     "Set as default" "デフォルトとして設定"
     "Set" "設定"
     "Shape name: " "図形名: "
     "Shape" "図形"
     "Show SVG" "SVGを表示"
     "Special Preset" "特殊プリセット"
     "Split Path at Point" "この点でパスを分割"
     "Split Subpath at Point" "この点でサブパスを分割"
     "Split Subpaths" "サブパス毎に分割"
     "Start Marker" "始点マーカー"
     "Stroke" "線"
     "Stroke..." "線..."
     "Support for `style' attributes is insufficient and may cause display and operation problems" "style属性のサポートは不十分であり、表示や操作に支障を来す可能性があります"
     "Temporary State" "一時状態"
     "Text Tool" "テキストツール"
     "Text" "テキスト"
     "Text..." "テキスト..."
     "Text: " "テキスト: "
     "The buffer has been killed" "バッファが既に削除されています"
     "The crop range is empty." "切り抜き範囲が空です。"
     "The empty shapes cannot be transformed" "空の図形は変形できません"
     "The extension is not .edraw.svg" "拡張子が .edraw.svg ではありません"
     "The link at point is not of type `file:'" "ポイントにあるリンクが `file:' タイプではありません"
     "The operation is not supported on this object" "このオブジェクトではその操作はサポートされていません"
     "The root entry cannot be deleted" "ルート項目は削除できません"
     "This shape picker is not connected to an editor" "このシェイプピッカーはエディタと接続されていません"
     "To Frame" "フレーム化"
     "To Window" "ウィンドウ化"
     "Top Most" "最前面"
     "Transform Method" "変形方式"
     "Transform" "変形"
     "Transform..." "変形..."
     "Translate All..." "全平行移動..."
     "Translate..." "平行移動..."
     "Transparent BG" "透明背景"
     "Unable to cut root entry" "ルート項目はカットできません"
     "Undo" "取り消し"
     "Unglue All" "全接着解除"
     "Unglue" "接着解除"
     "Ungroup" "グループ解除"
     "Unknown type of shape definition" "知らない図形型"
     "Unsupported SVG element: %s" "未対応のSVG要素: %s"
     "Unsupported path command: `%s'" "未対応のパスコマンド: `%s'"
     "Unsupported unit" "未対応の単位"
     "View Box..." "viewBox=..."
     "View Height: " "表示高さ: "
     "View Width: " "表示幅: "
     "View" "表示"
     "WARNING: Images in other directories cannot be displayed for security reasons" "警告: 他のディレクトリにある画像はセキュリティ上の理由で表示できません"
     "Wavelength: " "波長: "
     "Write edraw file: " "出力edrawファイル: "
     "Wavy Line" "波線"
     "X Interval: " "X 間隔: "
     "X Maximum: " "X 最大値: "
     "X Minimum: " "X 最小値: "
     "X: " "X: "
     "Y Interval: " "Y 間隔: "
     "Y Maximum: " "Y 最大値: "
     "Y Minimum: " "Y 最小値: "
     "Y: " "Y: "
     "Yes" "はい"
     "Z-Order" "重ね順"
     "Zigzag Line" "ジグザグ線"
     "Zoom In" "ズームイン"
     "Zoom Out" "ズームアウト"
     "[Custom Shape Tool] Click:Add shape(original size), Drag:Add shape(specified size), S-Drag:Square" "[カスタム図形ツール] クリック:図形追加(元サイズ), ドラッグ:図形追加(指定サイズ), S-ドラッグ:正方形指定"
     "[Ellipse Tool] Drag:Add ellipse, S-Drag:Square" "[楕円ツール] ドラッグ:楕円追加, S-ドラッグ:正方形指定"
     "[Freehand Tool] Drag:Add path" "[手書きツール] ドラッグ:パス追加"
     "[Generator Tool] Click:Add generator shape" "[生成ツール] Click:生成図形を追加"
     "[Image Tool] Click:Add image(original size), Drag:Add image(specified size), S-Drag:Square" "[画像ツール] クリック:画像追加(元サイズ), ドラッグ:画像追加(指定サイズ), S-ドラッグ:正方形指定"
     "[Path Tool] Click:Add Anchor, Drag:Add Anchor and Handles, a:New Path
(On Endpoint) Click:Continue/Connect, C-u Click:Add Anchor
(On Point) Click:Select, Drag:Move / (On Handle) M-Drag:Move
(On Anchor) M-Click:Make Corner, M-Drag:Recreate Handles
(On Another Shape) C-Click:Glue / S-Click/drag: Limit to 45 degrees" "[パスツール] クリック:アンカー追加, ドラッグ:アンカーとハンドル追加, a:新パス
(端点上) クリック:継続・接続, C-u クリック:アンカー追加(接続回避)
(点上) クリック:選択, ドラッグ:移動 / (ハンドル点上) M-ドラッグ:移動
(アンカー点上) M-クリック:角にする, M-ドラッグ:ハンドル再作成
(他図形上) C-クリック:接着 / S-クリック・ドラッグ: 45度単位指定"
     "[Rect Tool] Drag:Add rect, S-Drag:Square" "[矩形ツール] ドラッグ:矩形追加, S-ドラッグ:正方形指定"
     "[Select Tool] Click:Select, Drag:Range select or Move, M-Drag:Duplicate and move, S-Click:45-degree, Double Click:Properties" "[選択ツール] クリック:選択, ドラッグ:範囲指定または移動, M-ドラッグ:複製移動, S-クリック:45度単位, ダブルクリック:プロパティエディタ"
     "[Text Tool] Click:Add or Change, C-u Click:Add, C-Click:Glue" "[テキストツール] クリック:テキスト追加・変更, C-u クリック:追加のみ C-クリック:接着"
     "\"transform\" Property" "\"transform\"プロパティ"
     "a:auto  t:transform property  p:anchor points" "a:自動  t:transformプロパティ  p:アンカーポイント"
     "all, none, property names separated by spaces, or empty: " "all, none, 空白区切りのプロパティ名列, or 空: "
     "edraw-import: %s warnings raised" "edraw-import: %s 件の警告が発生しました"
     "q/R-Click:Cancel, RET/Dbl-Click:Commit,\ns:Scale(%.2f%% %.2f%%), r:Rotate(%.2fdeg), t:Translate,\no:Origin(%s %s), m:Transform Method(%s)" "q/右クリック:キャンセル, RET/ダブルクリック:確定,\ns:拡大縮小(%.2f%% %.2f%%), r:回転(%.2f度), t:平行移動,\no:原点(%s %s), m:変形方式(%s)"
     "r-click:quit, drag:Scroll, wheel:Zoom,\nSPC/q/C-g:quit, [S|C|M-]arrow keys:Scroll, +/-:Zoom, 0:reset" "右クリック:終了, ドラッグ:スクロール, ホイール:ズーム,\nSPC/q/C-g:終了, [S|C|M-]矢印キー:スクロール, +/-:ズーム, 0:リセット"
     ;; [END MSG DATA]
     )))

;;(provide 'edraw-msg-ja)
;;; edraw-msg-ja.el ends here
