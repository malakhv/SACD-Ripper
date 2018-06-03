package com.malakhv.sripper.ui;

import javax.swing.*;

/**
 * The main window in this app.
 * @author Mikhail.Malakhov
 * */
public class WinMain extends JFrame {
    private JLabel lblDiscArtist;
    private JLabel lblDiscTitle;
    private JLabel lblDiscLocale;
    private JLabel lblDiscVersion;
    private JLabel lblDiscDate;
    private JLabel lblDiscNumber;
    private JLabel lblAlbumArtist;
    private JLabel lblAlbumTitle;
    private JLabel lblAlbumLocale;
    private JLabel lblAlbumSet;
    private JLabel lblAlbumNumber;
    private JPanel rootPanel;

    public WinMain() {
        setContentPane(rootPanel);
        setVisible(true);
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    }
}