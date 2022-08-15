/* *
 * Copyright (C) 2013 Mikhail Malakhov <malakhv@live.ru>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * */

package com.malakhv.sripper.sacd;

/**
 * The class describes base SACD Disk/Album information.
 * @author Mikhail.Malakhov
 * */
/* package */ class BaseInfo {

    /** The Disc/Album artist. Can be {@code null}. */
    public String artist;

    /** The Disc/Album title. Can be {@code null}. */
    public String title;

    /** The Disc/Album locale. Can be {@code null}. */
    public String locale;

    /** The Disc/Album number. Can be {@code null}. */
    public String number;

    /**
     * Clears all fields. By defaults, empty field has {@code null}.
     * */
    public void clear() {
        artist = title = locale = number = null;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "artist=" + artist + ", title=" + title + ", locale=" + locale + ", number=" + number;
    }
}