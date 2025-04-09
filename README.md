# markdown-to-org

这是一个 Emacs 包，用于将 Markdown 文本转换为 Org-mode 格式。

## 安装

### 使用 MELPA

如果你使用 MELPA，可以通过以下命令安装：

```elisp
M-x package-install RET markdown-to-org RET
```

### 手动安装

1. 克隆此仓库
2. 将文件添加到你的 `load-path`
3. 在 init.el 中添加：
```elisp
(require 'markdown-to-org)
```

## 使用方法

1. 在 Emacs 中打开一个包含 Markdown 文本的缓冲区
2. 选择要转换的文本区域
3. 运行 `M-x process-region-with-python-preset`
4. 从菜单中选择 "md2org"
5. 按照提示输入缩进级别和起始标题级别
6. 转换后的文本将显示在新的缓冲区中

## 功能

- 将 Markdown 转换为 Org-mode 格式
- 支持自定义缩进级别
- 支持自定义起始标题级别
- 提供其他文本处理功能（如大小写转换、单词计数等）

## 依赖

- Emacs 24.1 或更高版本
- Org-mode 9.0 或更高版本
- ob-python 0.1 或更高版本

## 许可证

GNU General Public License v3.0 